package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.VocabularyData
import com.github.mlangc.memento.db.model.Word

case class TrainingData(translations: Vector[Translation], synonyms: Map[Word, Set[Word]])

object TrainingData {
  def convert(data: VocabularyData): TrainingData = {
    TrainingData(data.translations.toVector, Map())
  }
}

