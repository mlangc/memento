package com.github.mlangc.memento.trainer.examiner

import java.time.Instant
import java.util.concurrent.TimeUnit

import cats.instances.option._
import cats.syntax.option._
import cats.syntax.traverse._
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.VocabularyData
import com.github.mlangc.memento.trainer.examiner.DefaultExaminer.ExamState
import com.github.mlangc.memento.trainer.model._
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.slf4zio.api._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import zio.Managed
import zio.Queue
import zio.Ref
import zio.Schedule
import zio.Semaphore
import zio.Task
import zio.UIO
import zio.ZSchedule
import zio.clock.Clock
import zio.duration.Duration
import zio.interop.catz._

object DefaultExaminer {
  private case class ExamState(lastQuestion: Option[Question] = None,
                               lastAnswer: Option[Answer] = None,
                               lastCheck: Option[Check] = None,
                               spellingHinter: Option[SpellingHinter] = None)
}

class DefaultExaminer(repetitionScheme: RepetitionScheme) extends Examiner with LoggingSupport {
  def prepareExam(db: VocabularyDb): Managed[Throwable, Option[Exam]] =
    Managed.fromEffect(db.load).flatMap(data => mkExam(db, data))

  private def mkExam(db: VocabularyDb, data: VocabularyData): Managed[Throwable, Option[Exam]] = {
    def addChecksTillNone(queue: Queue[Option[Check]], reportIssue: TechnicalIssue => UIO[Unit]): Task[Unit] =
      queue.take.flatMap {
        case None => Task.unit
        case Some(check) => keepTryingAddCheck(db, check, reportIssue) *> addChecksTillNone(queue, reportIssue)
      }

    val checksQueueWithWorkerFiber: Managed[Throwable, (Queue[Option[Check]], Ref[Option[TechnicalIssue]])] = {
      def acquire: UIO[(Queue[Option[Check]], Ref[Option[TechnicalIssue]])] = for {
        queue <- Queue.bounded[Option[Check]](1)
        issueRef <- Ref.make(none[TechnicalIssue])
        _ <- addChecksTillNone(queue, issue => issueRef.set(issue.some)).fork
      } yield (queue, issueRef)

      def release(queueWithRef: (Queue[Option[Check]], Ref[Option[TechnicalIssue]])): UIO[Unit] =
        queueWithRef._1.offer(None).unit

      Managed.make(acquire)(release)
    }

    checksQueueWithWorkerFiber.mapM { case (queue, technicalIssueRef) =>
      val trainingData = TrainingData.convert(data)

      for {
        maybeSchemeImpl <- repetitionScheme.implement(trainingData)
        exam <- maybeSchemeImpl.map { schemeImpl =>
          for {
            examStateRef <- Ref.make(ExamState())
            semaphore <- Semaphore.make(1)
          } yield {
            val shouldStop = schemeImpl.status.map(_.shouldStop)
            val technicalIssue = technicalIssueRef.get.tap(_ => technicalIssueRef.set(None))
            Exam.create(data)(shouldStop, technicalIssue) { (ask: Question => Task[Option[Answer]]) =>
              semaphore.withPermit {
                for {
                  state <- examStateRef.get
                  questionAndHinter <- nextQuestion(state, schemeImpl)
                  (question, hinter) = questionAndHinter
                  answer <- ask(question)
                  feedback = answer.map(giveFeedback(question, trainingData.synonyms))
                  check <- safeFeedback(question, feedback, queue)
                  _ <- examStateRef.update(updateExamState(question, answer, hinter, check))
                  _ <- check.traverse(schemeImpl.update)
                } yield (question, feedback)
              }
            }.some // it should be possible to use traverse here
          }
        } getOrElse(UIO.succeed(None))
      } yield exam
    }
  }

  private def keepTryingAddCheck(db: VocabularyDb, check: Check, reportIssue: TechnicalIssue => UIO[Unit]): Task[Unit] = {
    val tryAddCheck: UIO[Boolean] = db.addCheck(check).const(true).catchAll {
      case throwable: Throwable =>
        val issue = TechnicalIssue.fromString(throwable.toString)
            .getOrElse(TechnicalIssue(Refined.unsafeApply[String, NonEmpty](s"Got exception of type: ${throwable.getClass.getCanonicalName}")))

        logger.errorIO("Could not add check", throwable) *>
          reportIssue(issue).const(false)
    }

    val retrySchedule: Schedule[Boolean, Unit] = {
      Schedule.doUntil[Boolean](identity) &&
        (Schedule.exponential(Duration(250, TimeUnit.MILLISECONDS)) ||
          Schedule.spaced(Duration(10, TimeUnit.SECONDS)))
    }.unit


    tryAddCheck.repeat(retrySchedule).provide(Clock.Live).unit
  }

  private def updateExamState(question: Question,
                              maybeAnswer: Option[Answer],
                              hinter: Option[SpellingHinter],
                              maybeCheck: Option[Check])(state: ExamState): ExamState = {
    maybeAnswer.map { answer =>
      state.copy(
        lastQuestion = Some(question),
        lastAnswer = Some(answer),
        lastCheck = maybeCheck,
        spellingHinter = hinter)
    }.getOrElse(state)
  }

  private def nextQuestion(state: ExamState, schemeImpl: RepetitionScheme.Impl): Task[(Question, Option[SpellingHinter])] = {
    state match {
      case ExamState(Some(lastQuestion), Some(Answer.NeedHint), _, _) =>
        for {
          spellingHinter <- state.spellingHinter.map(Task.succeed).getOrElse {
            SpellingHinter.make(lastQuestion.rightAnswer.spelling)
          }
          nextQuestion <- spellingHinter.nextHint.map(hint => lastQuestion.copy(hint = Some(hint)))
        } yield (nextQuestion, Some(spellingHinter))

      case _ =>
        schemeImpl.next.map((_, None))
    }
  }

  private def toCheck(question: Question, feedback: Feedback): Task[Option[Check]] = feedback match {
    case Feedback.Correction(_, _, score) =>
      Task(Instant.now()).map {
        now =>
          Some(Check(question.translation, question.direction, score, now))
      }

    case _ => Task.succeed(None)
  }


  private def safeFeedback(question: Question,
                           maybeFeedback: Option[Feedback],
                           checksToAdd: Queue[Option[Check]]): Task[Option[Check]] = maybeFeedback match {
    case None => Task.succeed(None)
    case Some(feedback) =>
      for {
        check <- toCheck(question, feedback)
        _ <- check.map(check => checksToAdd.offer(check.some)).getOrElse(Task.unit)
      } yield check
  }

  private def giveFeedback(question: Question, synonyms: Synonyms)(answer: Answer): Feedback = answer match {
    case answer: ScorableAnswer =>
      val score = Examiner.score(question, answer, synonyms)
      Feedback.Correction(question.rightAnswer, answer.value, score)

    case _ => Feedback.Postponed
  }
}
