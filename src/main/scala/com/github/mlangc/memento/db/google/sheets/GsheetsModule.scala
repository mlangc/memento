package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.db.DbModule
import com.github.mlangc.memento.db.VocabularyDb
import scalaz.zio.Task

trait GsheetsModule extends DbModule {
  def secretsFile: File
  def vocabularyDb(sheetId: String): Task[VocabularyDb] = GsheetsVocabularyDb.make(sheetId, secretsFile)
}

