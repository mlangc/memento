package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.db.DbModule
import com.github.mlangc.memento.db.VocabularyDb
import zio.Task
import zio.blocking.Blocking

trait GsheetsModule extends DbModule {
  def secretsFile: File
  def tokensDir: File
  def vocabularyDb(sheetId: String): Task[VocabularyDb] = GsheetsVocabularyDb.make(sheetId, secretsFile, tokensDir).provide(Blocking.Live)
}

