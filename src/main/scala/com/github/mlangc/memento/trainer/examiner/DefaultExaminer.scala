package com.github.mlangc.memento.trainer.examiner

import java.time.Instant

import cats.syntax.option._
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.{Check, VocabularyData}
import com.github.mlangc.memento.trainer.examiner.DefaultExaminer.ExamState
import com.github.mlangc.memento.trainer.model._
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import zio.{Ref, Semaphore, Task, UIO}

object DefaultExaminer {
  private case class ExamState(lastQuestion: Option[Question] = None,
                               lastAnswer: Option[Answer] = None,
                               lastCheck: Option[Check] = None,
                               spellingHinter: Option[SpellingHinter] = None)
}

class DefaultExaminer(repetitionScheme: RepetitionScheme) extends Examiner {
  def prepareExam(db: VocabularyDb): Task[Option[Exam]] =
    db.load.flatMap(data => mkExam(db, data))

  private def mkExam(db: VocabularyDb, data: VocabularyData): Task[Option[Exam]] = {
    val trainingData = TrainingData.convert(data)

    for {
      maybeSchemeImpl <- repetitionScheme.implement(trainingData)
      exam <- maybeSchemeImpl.map { schemeImpl =>
        for {
          examStateRef <- Ref.make(ExamState())
          semaphore <- Semaphore.make(1)
        } yield {
          Exam.create(data) { (ask: Question => Task[Option[Answer]]) =>
            semaphore.withPermit {
              for {
                state <- examStateRef.get
                questionAndHinter <- nextQuestion(state, schemeImpl)
                (question, hinter) = questionAndHinter
                answer <- ask(question)
                feedback = answer.map(giveFeedback(question, trainingData.synonyms))
                check <- safeFeedback(db, question, feedback)
                _ <- examStateRef.update(updateExamState(question, answer, hinter, check))
              } yield (question, feedback)
            }
          }.some // it should be possible to use traverse here
        }
      } getOrElse(UIO.succeed(None))
    } yield exam
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

      case _ => {
        state.lastCheck match {
          case Some(check) =>
            schemeImpl.next(check)

          case _ =>
            schemeImpl.next
        }
      }.map((_, None))
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


  private def safeFeedback(db: VocabularyDb,
                           question: Question,
                           maybeFeedback: Option[Feedback]): Task[Option[Check]] = maybeFeedback match {
    case None => Task.succeed(None)
    case Some(feedback) =>
      for {
        check <- toCheck(question, feedback)
        _ <- check.map(db.addCheck).getOrElse(Task.unit)
      } yield check
  }

  private def giveFeedback(question: Question, synonyms: Synonyms)(answer: Answer): Feedback = answer match {
    case answer: ScorableAnswer =>
      val score = Examiner.score(question, answer, synonyms)
      Feedback.Correction(question.rightAnswer, answer.value, score)

    case _ => Feedback.Postponed
  }
}
