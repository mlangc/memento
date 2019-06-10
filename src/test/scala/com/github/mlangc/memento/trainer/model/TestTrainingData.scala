package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Translation

object TestTrainingData {
  val empty = TrainingData(Vector.empty, Map.empty)
  val enGerSingleElem = TrainingData(Vector(Translation("Nacht", "night")), Map.empty)

  val enGerTwoElems = enGerSingleElem.copy(
    translations = enGerSingleElem.translations :+ Translation("Tag", "day"))
}
