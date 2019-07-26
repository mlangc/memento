package com.github.mlangc.memento.trainer.model

import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString

case class TechnicalIssue(explanation: NonEmptyString, warnings: List[NonEmptyString] = Nil) {
  def addWarning(warning: String): TechnicalIssue =
    NonEmptyString.from(warning).toOption.map(w => copy(warnings = w :: warnings))
      .getOrElse(this)
}

object TechnicalIssue {
  def fromString(msg: String): Option[TechnicalIssue] =
    refineV[NonEmpty](msg).toOption.map(explanation => TechnicalIssue(explanation))
}
