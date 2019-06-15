package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.{Check, Translation, VocabularyData}

case class TrainingData(translations: Vector[Translation],
                        synonyms: Synonyms,
                        checks: List[Check])

object TrainingData {
  def convert(data: VocabularyData): TrainingData = {
    TrainingData(data.translations.toVector, Synonyms(Map(), Map()), Nil)
  }
}

