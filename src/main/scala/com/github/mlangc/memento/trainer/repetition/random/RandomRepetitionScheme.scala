package com.github.mlangc.memento.trainer.repetition.random

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import zio.{Ref, Task}

import scala.util.Random

object RandomRepetitionScheme extends RepetitionScheme {
  def implement(translations: NonEmptyVector[Translation], checks: List[Check]): Task[RepetitionScheme.Impl] =
    for {
      checksRef <- Ref.make(checks)
      scheme <- Task {
        new RepetitionScheme.Impl {
          def next: Task[Question] =
            for {
              ind <- Task(Random.nextInt(translations.length))
              translation = translations.getUnsafe(ind)
              direction <- Task(Random.nextBoolean()).map(Direction.fromBoolean)
              checks <- checksRef.get
            } yield Question.create(translation, direction, checks)

          def update(check: Check): Task[Unit] =
            checksRef.update(check :: _).unit

          def status: Task[RepetitionStatus] = Task.succeed(RepetitionStatus.ShouldContinue)
        }
      }
    } yield scheme
}
