package com.github.mlangc.memento.db.cache

import java.io.File

import com.github.mlangc.memento.db.google.sheets.SheetId
import zio.ZManaged
import zio.blocking.Blocking

trait CacheModule {
  def cache: CacheModule.Service
}

object CacheModule {
  trait Service {
    def makeSheetCache(baseDir: File, sheetId: SheetId): ZManaged[Blocking, Throwable, SimpleCache]
  }
}
