package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.db.model.LanguageName
import com.github.mlangc.memento.db.model.VocabularyData
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Feedback
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.TechnicalIssue
import com.github.mlangc.slf4zio.api._
import zio.Task

trait Exam {
  def nextQuestion(ask: Question => Task[Option[Answer]]): Task[(Question, Option[Feedback])]
  def language1: LanguageName
  def language2: LanguageName
  def shouldStop: Task[Boolean]
  def technicalIssue: Task[Option[TechnicalIssue]]
}

object Exam extends LoggingSupport {
  def create(vocabularyData: VocabularyData)
            (stop: Task[Boolean], issue: Task[Option[TechnicalIssue]])
            (next: (Question => Task[Option[Answer]]) => Task[(Question, Option[Feedback])])
  : Exam = new Exam {
    def nextQuestion(ask: Question => Task[Option[Answer]]): Task[(Question, Option[Feedback])] = next(ask)

    def language1: LanguageName = vocabularyData.language1
    def language2: LanguageName = vocabularyData.language2
    def shouldStop: Task[Boolean] = stop
    def technicalIssue: Task[Option[TechnicalIssue]] = issue
  }
}
