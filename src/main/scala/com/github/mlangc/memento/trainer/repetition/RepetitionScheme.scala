package com.github.mlangc.memento.trainer.repetition

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TrainingData
import zio.Task
import zio.ZIO

trait RepetitionScheme {
  final def implement(data: TrainingData): Task[Option[RepetitionScheme.Impl]] =
    ZIO.foreach(NonEmptyVector.fromVector(data.translations)) {
      ts => implement(ts, data.checks)
    }

  protected def implement(translations: NonEmptyVector[Translation], checks: List[Check]): Task[RepetitionScheme.Impl]
}

object RepetitionScheme {
  trait Impl {
    def next: Task[Question]
    def update(check: Check): Task[Unit]
    def status: Task[RepetitionStatus]
  }
}
