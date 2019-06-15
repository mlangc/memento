package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Translation

object TestTrainingData {
  val empty = TrainingData(Vector.empty, Synonyms(Map(), Map()), Nil)
  val enGerSingleElem = TrainingData(Vector(Translation("Nacht", "night")), Synonyms(Map(), Map()), Nil)

  val enGerTwoElems: TrainingData = enGerSingleElem.copy(
    translations = enGerSingleElem.translations :+ Translation("Tag", "day"))
}
