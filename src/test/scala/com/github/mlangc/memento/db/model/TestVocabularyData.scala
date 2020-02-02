package com.github.mlangc.memento.db.model

import eu.timepit.refined.auto._

object TestVocabularyData {
  val gerFrEmpty: VocabularyData = VocabularyData(
    language1 = "Deutsch",
    language2 = "Französisch",
    translations = Nil,
    synonyms1 = Nil,
    synonyms2 = Nil,
    checks = Nil
  )

  def gerFrSimple: VocabularyData = gerFrEmpty.copy(
    translations = Translation("lobend", "élogieux") ::
      Translation("verbreitet", "répandu") ::
      Translation("schmutzig", "crasseux") ::
      Translation("Abrechnung", "un décompte") ::
      Nil,

    checks = TestChecks.IchJe :: TestChecks.VerbreitetRepandu :: Nil
  )
}
