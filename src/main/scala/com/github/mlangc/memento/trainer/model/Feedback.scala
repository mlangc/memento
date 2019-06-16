package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Vocabulary
import com.github.mlangc.memento.trainer.model.Feedback.Correction

sealed trait Feedback {
  def maybeScore: Option[Score] = this match {
    case Correction(_, score) => Some(score)
    case _ => None
  }
}

object Feedback {
  case class Correction(word: Vocabulary, score: Score) extends Feedback
  object Postponed extends Feedback
}

