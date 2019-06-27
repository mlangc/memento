package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.db.mock.InMemoryVocabularyDb
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.TestVocabularyData
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Hint
import com.github.mlangc.memento.trainer.model.Question
import org.scalatest.OptionValues
import zio.Ref
import zio.Task

import eu.timepit.refined.auto._

abstract class GenericExaminerTest extends BaseZioTest with OptionValues {
  "Test with an empty db" inIO {
    val voxData = TestVocabularyData.gerFrEmpty
    for {
      db <- InMemoryVocabularyDb.make(voxData)
      exam <- examiner.prepareExam(db)
      _ <- Task(assert(exam.isEmpty))
    } yield ()
  }

  "Make sure that our examiner is sane" inIO {
    val voxData = TestVocabularyData.gerFrSimple
    for {
      db <- InMemoryVocabularyDb.make(voxData)
      exam <- examiner.prepareExam(db).flatMap(exam => Task(exam.value))
      _ <- Task {
        assert(exam.language1 === voxData.language1)
        assert(exam.language2 === voxData.language2)
      }

      _ <- exam.nextQuestion(_ => Task.succeed(Some(Answer.Blank))).flatMap { feedback =>
        Task {
          feedback match {
            case (_, Some(Feedback.Correction(_, _, Score.Zero))) => ()
            case _ => fail()
          }
        }
      }

      lastQuestionRef <- Ref.make[Option[Question]](None)
      _ <- exam.nextQuestion(q => lastQuestionRef.set(Some(q)) *> Task.succeed(Some(Answer.NeedHint))).flatMap { feedback =>
        Task {
          assert(feedback._2 === Some(Feedback.Postponed))
        }
      }

      lastHintRef <- Ref.make[Option[Hint]](None)
      feedbackAfterHint <- exam.nextQuestion { currentQuestion =>
        for {
          lastQuestion <- lastQuestionRef.get
          answer <- Task {
            val question = lastQuestion.value
            assert(question.translation === currentQuestion.translation)
            assert(currentQuestion.hint.nonEmpty)
            Some(Answer.NeedHint)
          }
          _ <- lastHintRef.set(currentQuestion.hint)
          _ <- lastQuestionRef.set(Some(currentQuestion))
        } yield answer
      }

      feedbackAfterAnswer <- exam.nextQuestion { currentQuestion =>
        for {
          lastHint <- lastHintRef.get
          lastQuestion <- lastQuestionRef.get
          answer <- Task {
            assert(currentQuestion.hint.nonEmpty)
            assert(lastQuestion.value.translation === currentQuestion.translation)
            assert(lastHint !== currentQuestion.hint)
            Some(Answer.Text(currentQuestion.rightAnswer.spelling))
          }
        } yield answer
      }

      _ <- Task {
        feedbackAfterAnswer._2.value match {
          case Feedback.Correction(_, _, score) =>
            assert(score !== Score.Perfect)
            assert(score !== Score.Zero)

          case _ =>
            fail("" + feedbackAfterHint._2.value)
        }
      }
    } yield ()
  }

  protected def examiner: Examiner
}
