package com.github.mlangc.memento.db.model

case class Translation(left: Word, right: Word)

object Translation {
  def apply(left: String, right: String): Translation = Translation(Word(left), Word(right))
}

