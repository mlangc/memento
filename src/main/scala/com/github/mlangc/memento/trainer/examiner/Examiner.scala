package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.ScorableAnswer
import com.github.mlangc.memento.trainer.model.Synonyms
import scalaz.zio.Task

trait Examiner {
  def prepareExam(db: VocabularyDb): Task[Option[Exam]]
}

object Examiner {
  def score(question: Question, answer: ScorableAnswer, hintsSeenBefore: Int, synonyms: Synonyms): Score = answer match {
    case Answer.Blank => Score.Zero
    case Answer.Text(text) =>
      val rightAnswer = question.rightAnswer.spelling
      val hintsTotal = hintsSeenBefore + question.hint.map(_ => 1).getOrElse(0)
      val initialScore = if (text == rightAnswer) Score.Perfect else {
        Score.Zero
      }

      if (synonyms.left.nonEmpty || synonyms.right.nonEmpty)
        ???

      updateScoreConsideringHints(initialScore, hintsTotal)
  }

  private def updateScoreConsideringHints(score: Score, numHints: Int): Score = {
    if (numHints <= 0) score else {
      val scoreOrd = score match {
        case Score.Zero => 0
        case Score.Poor => 1
        case Score.SoSo => 2
        case Score.Good => 3
        case Score.Perfect => 4
      }

      scoreOrd - numHints match {
        case ord if ord <= 0 => Score.Zero
        case 1 => Score.Poor
        case 2 => Score.SoSo
        case 3 => Score.Good
        case _ => Score.Perfect
      }
    }
  }
}
