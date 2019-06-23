package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.RevealedRatio
import com.github.mlangc.memento.trainer.model.ScorableAnswer
import com.github.mlangc.memento.trainer.model.Synonyms
import eu.timepit.refined.auto._
import info.debatty.java.stringsimilarity.NormalizedLevenshtein
import zio.Task

trait Examiner {
  def prepareExam(db: VocabularyDb): Task[Option[Exam]]
}

object Examiner {
  private val stringSimilarity = new NormalizedLevenshtein

  def score(question: Question, answer: ScorableAnswer, synonyms: Synonyms): Score = answer match {
    case Answer.Blank => Score.Zero
    case Answer.Text(text) =>
      val rightAnswers = {
        val syns = if (question.direction == Direction.LeftToRight) synonyms.right else synonyms.left
        syns.getOrElse(question.rightAnswer, Set.empty) + question.rightAnswer
      }

      val revealedRatio = question.hint.map(_.revealed).getOrElse(0.0: RevealedRatio)
      val initialScore = rightAnswers.map(rightAnswer => score(text, rightAnswer.spelling.value)).max
      updateScoreConsideringHints(initialScore, revealedRatio)
  }

  private def score(given: String, rightAnswer: String): Score = {
    if (given == rightAnswer) Score.Perfect else {
      val givenLower = given.toLowerCase
      val rightLower = rightAnswer.toLowerCase

      if (givenLower == rightLower) Score.Good else {
        val lambda = {
          val cap = 0.57
          val score = stringSimilarity.similarity(givenLower, rightLower) - cap
          if (score <= 0) 0.0 else score * 1.0/cap
        }

        (lambda * 3).round match {
          case 1 => Score.Poor
          case 2 => Score.SoSo
          case 3 => Score.Good
          case _ => Score.Zero
        }
      }
    }
  }

  private def updateScoreConsideringHints(score: Score, revealedRatio: RevealedRatio): Score = {
    if (revealedRatio <= 0) score else {
      val scoreOrd = score match {
        case Score.Zero => 0
        case Score.Poor => 1
        case Score.SoSo => 2
        case Score.Good => 3
        case Score.Perfect => 4
      }

      val penalty = (revealedRatio * 4 * 1.8).round

      scoreOrd - penalty match {
        case ord if ord <= 0 => Score.Zero
        case 1 => Score.Poor
        case 2 => Score.SoSo
        case 3 => Score.Good
        case _ => Score.Perfect
      }
    }
  }
}
