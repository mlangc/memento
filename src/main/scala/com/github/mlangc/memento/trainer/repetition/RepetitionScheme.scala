package com.github.mlangc.memento.trainer.repetition

import cats.data.NonEmptyVector
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TrainingData
import scalaz.zio.Task
import scalaz.zio.interop.catz._
import cats.syntax.traverse._
import cats.instances.option._
import com.github.mlangc.memento.db.model.Check

trait RepetitionScheme {
  final def implement(data: TrainingData): Task[Option[RepetitionScheme.Impl]] =
    NonEmptyVector.fromVector(data.translations)
      .traverse(ts => implement(ts, data.checks))

  protected def implement(translations: NonEmptyVector[Translation], checks: List[Check]): Task[RepetitionScheme.Impl]
}

object RepetitionScheme {
  trait Impl {
    def next: Task[Question]
    def next(check: Check): Task[Question]
    def status: Task[RepetitionStatus]
  }
}
