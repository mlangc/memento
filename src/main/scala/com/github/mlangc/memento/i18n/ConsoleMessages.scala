package com.github.mlangc.memento.i18n

import java.time.Instant

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

trait ConsoleMessages {
  def noDataToTrainOn: String
  def pressEnterToContinue: String
  def timesAskedBefore(n: Int Refined NonNegative): String
  def lastAsked(instant: Option[Instant]): String
  def hint(hintStr: String): String
  def help(quitCmd: String, hintCmd: String): String
}
