package com.github.mlangc.memento.db

import zio.Task

trait DbModule {
  def vocabularyDb: Task[VocabularyDb]
}
