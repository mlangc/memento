package com.github.mlangc.memento.trainer.repetition

import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.CardsLeft
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldContinue
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldStop
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

sealed trait RepetitionStatus {
  def shouldContinue: Boolean = this match {
    case CardsLeft(_) | ShouldContinue => true
    case ShouldStop => false
  }

  def shouldStop: Boolean = !shouldContinue
}

object RepetitionStatus {
  case class CardsLeft(n: Int Refined Positive) extends RepetitionStatus
  case object ShouldStop extends RepetitionStatus
  case object ShouldContinue extends RepetitionStatus
}
