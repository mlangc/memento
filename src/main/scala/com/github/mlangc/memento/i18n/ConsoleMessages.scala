package com.github.mlangc.memento.i18n

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

trait ConsoleMessages {
  def noDataToTrainOn: String
  def pressEnterToContinue: String
  def timesAskedBefore(n: Int Refined NonNegative): String
  def hint(hintStr: String): String
}
