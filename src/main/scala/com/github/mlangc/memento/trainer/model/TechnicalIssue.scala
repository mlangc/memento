package com.github.mlangc.memento.trainer.model

import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import eu.timepit.refined.collection.NonEmpty

case class TechnicalIssue(explanation: String Refined NonEmpty)

object TechnicalIssue {
  def fromString(msg: String): Option[TechnicalIssue] =
    refineV[NonEmpty](msg).toOption.map(TechnicalIssue.apply)
}
