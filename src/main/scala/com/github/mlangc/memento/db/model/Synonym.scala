package com.github.mlangc.memento.db.model

case class Synonym(word1: Vocabulary, word2: Vocabulary)

object Synonym {
  def apply(word1: String, word2: String): Synonym = Synonym(Vocabulary(word1), Vocabulary(word2))
}
