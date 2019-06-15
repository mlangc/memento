package com.github.mlangc.memento.trainer.repetition

import java.time.Instant

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.db.model.{Check, Score}
import com.github.mlangc.memento.trainer.model.{Question, TestTrainingData, TrainingData}
import org.scalatest.OptionValues
import scalaz.zio.{Task, UIO, ZIO}

abstract class GenericRepetitionSchemeTest extends BaseZioTest with OptionValues {
  protected def scheme: RepetitionScheme

  protected final def implFor(data: TrainingData): Task[RepetitionScheme.Impl] =
    implFor(scheme, data)

  protected final def implFor(scheme: RepetitionScheme, data: TrainingData): Task[RepetitionScheme.Impl] =
    scheme.implement(data).flatMap(impl => Task(impl.value))

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
          question2 <- impl.next(check1)
          _ <- Task {
            assert(statusAtFirst.shouldContinue)
            assert(!statusAtFirst.shouldStop)
            assert(question1.translation === question2.translation)
          }
        } yield ()
      }

      "a two elem db" inIO {
        for {
          impl <- implFor(TestTrainingData.enGerTwoElems)
          statusAtFirst <- impl.status
          question0 <- impl.next
          questions <- ZIO.foldLeft(1.to(250))(question0 :: Nil) { (qs, _) =>
            toCheck(qs.head, Score.Perfect).flatMap(impl.next).map(_ :: qs)
          }
          _ <- Task {
            assert(statusAtFirst.shouldContinue)
            assert(questions.toSet.size === 4)
          }
        } yield ()
      }
    }
  }

  private def getTime: UIO[Instant] = UIO(Instant.now())

  private def toCheck(question: Question, score: Score): UIO[Check] =
    for {
      now <- getTime
    } yield Check(question.translation, question.direction, score, now)

}
