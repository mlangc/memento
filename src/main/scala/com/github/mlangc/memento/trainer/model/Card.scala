package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.{Check, Direction, Translation}

case class Card(translation: Translation, direction: Direction) {
  def correspondsTo(check: Check): Boolean =
    check.translation == translation && check.direction == direction

  def correspondsTo(question: Question): Boolean =
    question.translation == translation && question.direction == direction
}
