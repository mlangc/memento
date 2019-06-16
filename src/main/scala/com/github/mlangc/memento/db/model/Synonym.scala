package com.github.mlangc.memento.db.model

case class Synonym(voc1: Vocabulary, voc2: Vocabulary)

object Synonym {
  def apply(voc1: String, voc2: String): Synonym = Synonym(Vocabulary(voc1), Vocabulary(voc2))
}
