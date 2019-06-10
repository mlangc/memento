package com.github.mlangc.memento.db

import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.VocabularyData
import scalaz.zio.Task

trait VocabularyDb {
  def load: Task[VocabularyData]
  def addCheck(check: Check): Task[Unit]
}
