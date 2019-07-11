package com.github.mlangc.memento.i18n.de

import java.time.LocalDateTime

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.i18n.ConsoleMessages
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

class DeConsoleMessages extends ConsoleMessages {
  def noDataToTrainOn = "Keine Daten zum trainieren"
  def pressEnterToContinue = "Drücken Sie Eingabe um fortzufahren"
  def timesAskedBefore(n: Refined[Int, NonNegative]) = s"Bereits abgefragt: ${n.value} mal"
  def hint(hintStr: String) = s"Hinweis: $hintStr"

  def lastAsked(instant: Option[LocalDateTime]): String = {
    val tsStr = instant.map(_.toString).getOrElse("nie")
    s"Zuletzt gefragt: $tsStr"
  }

  def help(quitCommand: String, hintCommand: String, reloadCmd: String) =
    s"Verwenden Sie $quitCommand zum Aussteigen, $hintCommand für einen Hinweis oder $reloadCmd zum neu Laden"

  def correctAnswerWithScore(percentageRevealed: Int, score: Score): String =
    s"$score: You gave the right answer, but only after $percentageRevealed% have been revealed"

  def wrongAnswerWithScoreRevealed(expected: String, got: String, diff: String, percentageRevealed: Int, score: Score): String =
    s"$score[$diff]: Expected '$expected' but got '$got' after $percentageRevealed% have been reveled"

  def wrongAnswerWithScore(expected: String, got: String, diff: String, score: Score): String =
    s"$score[$diff]: Expected '$expected' but got '$got'"

  def allDoneContinueAnyway = "All done, do you want to continue anyway?"
  def promptYesNoDefaultingToNo = "yN >"
  def isYes(s: String): Boolean = s.toLowerCase == "y"

  def reloading = "Neu laden..."
}
