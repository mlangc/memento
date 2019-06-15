package com.github.mlangc.memento.trainer.repetition.leitner

import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation

private[leitner] case class Card(translation: Translation, direction: Direction) {
  def correspondsTo(check: Check): Boolean =
    check.translation == translation && check.direction == direction
}

