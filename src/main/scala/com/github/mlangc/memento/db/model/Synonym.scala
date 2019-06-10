package com.github.mlangc.memento.db.model

case class Synonym(word1: Word, word2: Word)

object Synonym {
  def apply(word1: String, word2: String): Synonym = Synonym(Word(word1), Word(word2))
}
