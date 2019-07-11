package com.github.mlangc.memento.i18n.en

import java.time.LocalDateTime

import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.i18n.ConsoleMessages
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

class EnConsoleMessages extends ConsoleMessages {
  def noDataToTrainOn = "No data to train on"
  def pressEnterToContinue = "Press enter to continue"
  def timesAskedBefore(n: Refined[Int, NonNegative]) = s"Times asked before: ${n.value}"
  def hint(hintStr: String) = s"Hint: $hintStr"

  def lastAsked(instant: Option[LocalDateTime]): String = {
    val tsStr = instant.map(_.toString).getOrElse("never")
    s"Last asked: $tsStr"
  }

  def help(quitCommand: String, hintCommand: String, reloadCmd: String) =
    s"Enter $quitCommand to quit, $hintCommand to get a hint or $reloadCmd to reload"

  def correctAnswerWithScore(percentageRevealed: Int, score: Score): String =
    s"$score: You gave the right answer, but only after $percentageRevealed% have been revealed"

  def wrongAnswerWithScoreRevealed(expected: String, got: String, diff: String, percentageRevealed: Int, score: Score): String =
    s"$score[$diff]: Expected '$expected' but got '$got' after $percentageRevealed% have been reveled"

  def wrongAnswerWithScore(expected: String, got: String, diff: String, score: Score): String =
    s"$score[$diff]: Expected '$expected' but got '$got'"

  def allDoneContinueAnyway = "All done, do you want to continue anyway?"
  def promptYesNoDefaultingToNo = "yN > "
  def isYes(s: String): Boolean = s.toLowerCase == "y"
  def reloading = "Reloading..."
}
