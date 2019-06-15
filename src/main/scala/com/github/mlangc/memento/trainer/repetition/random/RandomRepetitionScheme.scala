package com.github.mlangc.memento.trainer.repetition.random

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus
import scalaz.zio.Task

import scala.util.Random

object RandomRepetitionScheme extends RepetitionScheme {
  def implement(translations: NonEmptyVector[Translation], checks: List[Check]): Task[RepetitionScheme.Impl] = Task {
    new RepetitionScheme.Impl {
      def next: Task[Question] =
        for {
          ind <- Task(Random.nextInt(translations.length))
          translation = translations.getUnsafe(ind)
          direction <- Task(Random.nextBoolean()).map(Direction.fromBoolean)
        } yield Question(translation, direction)

      def next(check: Check): Task[Question] = next

      def status: Task[RepetitionStatus] = Task.succeed(RepetitionStatus.ShouldContinue)
    }
  }
}
