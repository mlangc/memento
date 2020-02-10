package com.github.mlangc.memento.db.model

case class Vocabulary(spelling: Spelling)

object Vocabulary {
  implicit val vocabularyOrdering: Ordering[Vocabulary] = Ordering.by(_.spelling.value)
}
