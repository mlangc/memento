package com.github.mlangc.memento.trainer.repetition.random

import com.github.mlangc.memento.trainer.model.TrainingData
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import scalaz.zio.Task

class RandomRepetitionScheme extends RepetitionScheme {
  def implement(data: TrainingData): Task[Option[RepetitionScheme.Impl]] = ???
}
