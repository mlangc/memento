package com.github.mlangc.memento.db.model

case class Synonym(voc1: Vocabulary, voc2: Vocabulary)

object Synonym {
  def apply(voc1: Spelling, voc2: Spelling): Synonym = Synonym(Vocabulary(voc1), Vocabulary(voc2))
}
