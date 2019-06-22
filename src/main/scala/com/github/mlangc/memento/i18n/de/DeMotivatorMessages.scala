package com.github.mlangc.memento.i18n.de

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.i18n.MotivatorMessages
import com.github.mlangc.memento.trainer.repetition.leitner.CardState

import scala.concurrent.duration.Duration

class DeMotivatorMessages extends MotivatorMessages {
  def boxInfo(index: Int,
              minScore: Score,
              interval: Duration,
              nextMinScore: Score,
              nextInterval: Duration,
              canAdvance: Boolean): String =
    s"""${boxInfo(index, minScore, interval)}
       |Nächste Box : (${index+1}, ${nextMinScore}, ${nextInterval}) (${if (canAdvance) "" else "noch nicht"} erreichbar
       |""".stripMargin

  def boxInfo(index: Int,
              minScore: Score,
              interval: Duration): String =
    s"Akutelle Box: ($index, $minScore, $interval)"

  def cardState(cardState: CardState) = s"Kartenstatus: $cardState"
}
