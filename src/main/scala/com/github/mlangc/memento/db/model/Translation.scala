package com.github.mlangc.memento.db.model

case class Translation(left: Vocabulary, right: Vocabulary)

object Translation {
  def apply(left: String, right: String): Translation = Translation(Vocabulary(left), Vocabulary(right))
}

