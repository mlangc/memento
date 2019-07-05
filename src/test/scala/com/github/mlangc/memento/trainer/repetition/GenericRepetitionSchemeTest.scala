package com.github.mlangc.memento.trainer.repetition

import java.time.Instant

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TestTrainingData
import com.github.mlangc.memento.trainer.model.TrainingData
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import org.scalatest.OptionValues
import zio.Task
import zio.UIO
import zio.ZIO

abstract class GenericRepetitionSchemeTest extends BaseZioTest with OptionValues {
  protected def scheme: RepetitionScheme

  protected final def implFor(data: TrainingData): Task[RepetitionScheme.Impl] =
    implFor(scheme, data)

  protected final def implFor(scheme: RepetitionScheme, data: TrainingData): Task[RepetitionScheme.Impl] =
    scheme.implement(data).flatMap(impl => Task(impl.value))

  protected final def updateAndNext(schemeImpl: RepetitionScheme.Impl, check: Check): Task[Question] =
    schemeImpl.update(check) *> schemeImpl.next

  "Repeating without any prior training" - {
    "trivial cases" - {
      "an empty db" inIO {
        for {
          impl <- scheme.implement(TestTrainingData.empty)
          _ <- Task(assert(impl.isEmpty))
        } yield ()
      }

      "a single elem db" inIO {
        for {
          impl <- implFor(TestTrainingData.enGerSingleElem)
          statusAtFirst <- impl.status
          question1 <- impl.next
          check1 <- toCheck(question1, Score.Good)
          question2 <- impl.update(check1) *> impl.next
          _ <- Task {
            assert(statusAtFirst.shouldContinue)
            assert(question1.timesAskedBefore.value === 0)
            assert(question1.lastAsked.isEmpty)
            assert(!statusAtFirst.shouldStop)
            assert(question1.translation === question2.translation)
          }
        } yield ()
      }

      "a two elem db" inIO {
        for {
          impl <- implFor(TestTrainingData.enGerTwoElems)
          statusAtFirst <- impl.status
          questions <- runSimulation(impl, 250)
          _ <- Task {
            val questionsWhereTimesAskedSeemsBroken = questions.groupBy(_.toCard)
              .mapValues(_.map(_.timesAskedBefore.value).toSet)
              .mapValues(timesAskedSet => 0.until(timesAskedSet.size).toSet == timesAskedSet)
              .filter(!_._2)

            assert(questionsWhereTimesAskedSeemsBroken.isEmpty)
            assert(statusAtFirst.shouldContinue)
            assert(questions.map(_.toCard).toSet.size === 4)
          }
        } yield ()
      }
    }
  }

  "Repeating with prior training" - {
    "simple cases" - {
      "a one translation db with a single check, that is obsolete" inIO {
        val obsoleteCheck = Check(Translation("Hoffnung", "Hope"), Direction.LeftToRight, Score.Good, Instant.EPOCH)
        val trainingData = TestTrainingData.enGerSingleElem.copy(checks = obsoleteCheck :: Nil)

        for {
          impl <- implFor(trainingData)
          questions <- runSimulation(impl, 250)
          _ <- Task(assert(questions.map(_.toCard).toSet.size === 2))
        } yield ()
      }
    }
  }

  private def runSimulation(impl: RepetitionScheme.Impl, numQuestions: Int Refined Positive): Task[List[Question]] =
    for {
      question0 <- impl.next
      questions <- ZIO.foldLeft(1.to(numQuestions.value - 1))(question0 :: Nil) { (qs, _) =>
        toCheck(qs.head, Score.Perfect).flatMap(check => impl.update(check) *> impl.next).map(_ :: qs)
      }
    } yield questions


  private def getTime: UIO[Instant] = UIO(Instant.now())

  private def toCheck(question: Question, score: Score): UIO[Check] =
    for {
      now <- getTime
    } yield Check(question.translation, question.direction, score, now)

}
