package com.github.mlangc.memento.trainer.repetition

import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.Overtraining
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.QuestionsLeft
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldContinue
import com.github.mlangc.memento.trainer.repetition.RepetitionStatus.ShouldStop
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

sealed trait RepetitionStatus {
  def shouldContinue: Boolean = this match {
    case QuestionsLeft(_) | ShouldContinue => true
    case Overtraining(_) | ShouldStop => false
  }

  def shouldStop: Boolean = !shouldContinue
}

object RepetitionStatus {
  case class QuestionsLeft(n: Int Refined Positive) extends RepetitionStatus
  case class Overtraining(n: Int Refined Positive) extends RepetitionStatus
  case object ShouldStop extends RepetitionStatus
  case object ShouldContinue extends RepetitionStatus
}
