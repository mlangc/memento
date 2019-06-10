package com.github.mlangc.memento.trainer.repetition

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.model.{TestTrainingData, TrainingData}
import org.scalatest.OptionValues
import scalaz.zio.Task
import scalaz.zio.ZIO

abstract class GenericRepetitionSchemeTest extends BaseZioTest with OptionValues {
  protected def scheme: RepetitionScheme

  protected final def implFor(data: TrainingData): Task[RepetitionScheme.Impl] =
    scheme.implement(data).flatMap(impl => Task(impl.value))

  "Repeating without any prior training" - {
    "trivial cases" - {
      "an empty db" in {
        unsafeRun {
          for {
            impl <- scheme.implement(TestTrainingData.empty)
            _ <- Task(assert(impl.isEmpty))
          } yield ()
        }
      }

      "a single elem db" in {
        unsafeRun {
          for {
            impl <- implFor(TestTrainingData.enGerSingleElem)
            statusAtFirst <- impl.status
            question1 <- impl.next
            question2 <- impl.next(question1, Score.Good)
            _ <- Task {
              assert(statusAtFirst.shouldContinue)
              assert(!statusAtFirst.shouldStop)
              assert(question1.translation === question2.translation)
            }
          } yield ()
        }
      }

      "a two elem db" in {
        unsafeRun {
          for {
            impl <- implFor(TestTrainingData.enGerTwoElems)
            statusAtFirst <- impl.status
            question0 <- impl.next
            questions <- ZIO.foldLeft(1.to(250))(question0 :: Nil) { (qs, _) =>
              impl.next(qs.head, Score.Perfect).map(_ :: qs)
            }
            _ <- Task {
              assert(statusAtFirst.shouldContinue)
              assert(questions.toSet.size === 4)
            }
          } yield ()
        }
      }
    }
  }

}
