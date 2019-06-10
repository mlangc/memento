package com.github.mlangc.memento.trainer

import java.time.Instant

import cats.instances.option._
import cats.syntax.traverse._
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.VocabularyData
import com.github.mlangc.memento.trainer.DefaultExaminer.ExamState
import com.github.mlangc.memento.trainer.examiner.Exam
import com.github.mlangc.memento.trainer.examiner.Examiner
import com.github.mlangc.memento.trainer.examiner.SpellingHinter
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.ScorableAnswer
import com.github.mlangc.memento.trainer.model.Synonyms
import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import scalaz.zio.Ref
import scalaz.zio.Semaphore
import scalaz.zio.Task
import scalaz.zio.interop.catz._

object DefaultExaminer {
  private case class ExamState(lastQuestion: Option[Question] = None,
                               lastAnswer: Option[Answer] = None,
                               lastScore: Option[Score] = None,
                               spellingHinter: Option[SpellingHinter] = None)
}

class DefaultExaminer(repetitionScheme: RepetitionScheme) extends Examiner {
  def prepareExam(db: VocabularyDb): Task[Option[Exam]] =
    db.load.flatMap(data => mkExam(db, data))

  private def mkExam(db: VocabularyDb, data: VocabularyData): Task[Option[Exam]] = {
    val trainingData = TrainingData.convert(data)

    for {
      maybeSchemeImpl <- repetitionScheme.implement(trainingData)
      exam <- maybeSchemeImpl.traverse { schemeImpl =>
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
                feedback = answer.map(giveFeedback(question))
                _ <- safeFeedback(db, question, question.translation, feedback)
                _ <- examStateRef.update(updateExamState(question, answer, hinter, feedback))
              } yield feedback
            }
          }
        }
      }
    } yield exam
  }

  private def updateExamState(question: Question,
                              maybeAnswer: Option[Answer],
                              hinter: Option[SpellingHinter],
                              maybeFeedback: Option[Feedback])(state: ExamState): ExamState = {
    maybeAnswer.map { answer =>
      state.copy(
        lastQuestion = Some(question),
        lastAnswer = Some(answer),
        lastScore = maybeFeedback.flatMap(_.maybeScore),
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
        (state.lastQuestion, state.lastScore) match {
          case (Some(lastQuestion), Some(lastScore)) =>
            schemeImpl.next(lastQuestion, lastScore)

          case _ =>
            schemeImpl.next
        }
      }.map((_, None))
    }
  }

  private def toCheck(translation: Translation, question: Question, feedback: Feedback): Task[Option[Check]] = feedback match {
    case Feedback.Correction(_, score) =>
      Task(Instant.now()).map {
        now =>
          Some(Check(translation, question.direction, score, now))
      }

    case _ => Task.succeed(None)
  }


  private def safeFeedback(db: VocabularyDb,
                           question: Question,
                           translation: Translation,
                           maybeFeedback: Option[Feedback]): Task[Unit] = maybeFeedback match {
    case None => Task.unit
    case Some(feedback) =>
      for {
        check <- toCheck(translation, question, feedback)
        _ <- check.map(db.addCheck).getOrElse(Task.unit)
      } yield ()
  }

  private def giveFeedback(question: Question)(answer: Answer): Feedback = answer match {
    case answer: ScorableAnswer =>
      val score = Examiner.score(question, answer, Synonyms.None)
      Feedback.Correction(question.rightAnswer, score)

    case _ => Feedback.Postponed
  }
}
