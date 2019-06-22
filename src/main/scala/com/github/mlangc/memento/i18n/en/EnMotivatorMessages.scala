package com.github.mlangc.memento.i18n.en

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.repetition.leitner.CardState

import scala.concurrent.duration.Duration

class EnMotivatorMessages extends MotivatorMessages {
  def boxInfo(index: Int,
              minScore: Score,
              interval: Duration,
              nextMinScore: Score,
              nextInterval: Duration,
              canAdvance: Boolean): String =
    s"""${boxInfo(index, minScore, interval)}
       |Next box   : ${renderBox(index + 1, nextMinScore, nextInterval)} (${if (canAdvance) "" else "not yet "}reachable)
       |""".stripMargin

  def boxInfo(index: Int,
              minScore: Score,
              interval: Duration): String =
    s"Current box: ${renderBox(index, minScore, interval)}"

  private def renderBox(index: Int, minScore: Score, interval: Duration): String =
    s"(index = $index, min score = $minScore, interval = $interval)"

  def cardState(cardState: CardState) = s"Card state: $cardState"
}
