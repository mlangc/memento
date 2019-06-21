package com.github.mlangc.memento.i18n.de

import com.github.mlangc.memento.i18n.ConsoleMessages
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

class DeConsoleMessages extends ConsoleMessages {
  def noDataToTrainOn = "Keine Daten zum trainieren"
  def pressEnterToContinue = "Drücken Sie Eingabe um fortzufahren"
  def timesAskedBefore(n: Refined[Int, NonNegative]) = s"Bereits abgefragt: ${n.value} mal"
  def hint(hintStr: String) = s"Hinweis: $hintStr"
}
