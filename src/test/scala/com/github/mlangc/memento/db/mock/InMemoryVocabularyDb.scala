package com.github.mlangc.memento.db.mock

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.VocabularyData
import scalaz.zio.Ref
import scalaz.zio.Task
import scalaz.zio.UIO

class InMemoryVocabularyDb(data: Ref[VocabularyData]) extends VocabularyDb {
  def load: Task[VocabularyData] = data.get

  def addCheck(check: Check): Task[Unit] =
    data.update { data =>
      data.copy(checks = data.checks :+ check)
    }.unit
}

object InMemoryVocabularyDb {
  def make(initialData: VocabularyData): UIO[InMemoryVocabularyDb] =
    Ref.make(initialData).map(new InMemoryVocabularyDb(_))
}
