package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.{Direction, Translation, Vocabulary}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

case class Question(translation: Translation,
                    direction: Direction,
                    timesAsked: Int Refined NonNegative,
                    hint: Option[Hint] = None) {
  def rightAnswer: Vocabulary = direction match {
    case Direction.LeftToRight => translation.right
    case Direction.RightToLeft => translation.left
  }

  def toCard: Card = Card(translation, direction)
}
