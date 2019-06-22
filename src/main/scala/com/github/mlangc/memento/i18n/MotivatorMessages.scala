package com.github.mlangc.memento.i18n

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.repetition.leitner.CardState

import scala.concurrent.duration.Duration

trait MotivatorMessages {
  def cardState(cardState: CardState): String
  def boxInfo(index: Int, minScore: Score, interval: Duration, nextMinScore: Score, nextInterval: Duration, canAdvance: Boolean): String
  def boxInfo(index: Int, minScore: Score, interval: Duration): String
}
