package com.github.mlangc.memento.db.model

case class Translation(left: Vocabulary, right: Vocabulary)

object Translation {
  def apply(left: Spelling, right: Spelling): Translation = Translation(Vocabulary(left), Vocabulary(right))

  implicit val translationOrdering: Ordering[Translation] = Ordering.by(t => (t.left, t.right))
}

