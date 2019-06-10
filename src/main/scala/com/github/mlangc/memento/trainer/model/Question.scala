package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.db.model.Word

case class Question(translation: Translation, direction: Direction, hint: Option[Hint] = None) {
  def rightAnswer: Word = direction match {
    case Direction.LeftToRight => translation.right
    case Direction.RightToLeft => translation.left
  }
}
