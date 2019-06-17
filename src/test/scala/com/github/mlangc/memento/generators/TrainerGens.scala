package com.github.mlangc.memento.generators

import com.github.mlangc.memento.trainer.model.TrainingData
import org.scalacheck.Gen

class TrainerGens(dbGens: DbGens = new DbGens()) {
  def trainingData: Gen[TrainingData] =
    dbGens.vocabularyData.map(TrainingData.convert)
}
