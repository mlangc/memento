package com.github.mlangc.memento.db

import scalaz.zio.Task

trait DbModule {
  def vocabularyDb: Task[VocabularyDb]
}
