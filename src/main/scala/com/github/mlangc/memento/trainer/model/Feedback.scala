package com.github.mlangc.memento.trainer.model

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Word

sealed trait Feedback

object Feedback {
  case class Correction(word: Word, score: Score) extends Feedback
  object Postponed extends Feedback
}

