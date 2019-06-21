package com.github.mlangc.memento.i18n.en

import com.github.mlangc.memento.i18n.ConsoleMessages
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

class EnConsoleMessages extends ConsoleMessages {
  def noDataToTrainOn = "No data to train on"
  def pressEnterToContinue = "Press enter to continue"
  def timesAskedBefore(n: Refined[Int, NonNegative]) = s"Times asked before: ${n.value}"
  def hint(hintStr: String) = s"hint: $hintStr"
}
