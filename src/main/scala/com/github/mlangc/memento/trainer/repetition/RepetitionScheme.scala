package com.github.mlangc.memento.trainer.repetition

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TrainingData
import scalaz.zio.Task

trait RepetitionScheme {
  def implement(data: TrainingData): Task[Option[RepetitionScheme.Impl]]
}

object RepetitionScheme {
  trait Impl {
    def next: Task[Question]
    def next(previousQuestion: Question, score: Score): Task[Question]
    def status: Task[RepetitionStatus]
  }
}
