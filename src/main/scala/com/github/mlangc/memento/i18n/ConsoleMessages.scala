package com.github.mlangc.memento.i18n

import java.time.LocalDateTime

import com.github.mlangc.memento.db.model.Score
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative

trait ConsoleMessages {
  def noDataToTrainOn: String
  def pressEnterToContinue: String
  def timesAskedBefore(n: Int Refined NonNegative): String
  def lastAsked(instant: Option[LocalDateTime]): String
  def hint(hintStr: String): String
  def help(quitCmd: String, hintCmd: String, reloadCmd: String): String

  def correctAnswerWithScore(percentRevealed: Int, score: Score): String
  def wrongAnswerWithScoreRevealed(expected: String, got: String, diff: String, percentageRevealed: Int, score: Score): String
  def wrongAnswerWithScore(expected: String, got: String, diff: String, score: Score): String

  def allDoneContinueAnyway: String
  def promptYesNoDefaultingToNo: String
  def isYes(s: String): Boolean
  def reloading: String
}
