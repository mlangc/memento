package com.github.mlangc.memento.db.google.sheets

import com.github.mlangc.memento.db.cache.CacheModule
import com.github.mlangc.slf4zio.api._
import zio.App
import zio.ZIO
import zio.ZEnv
import eu.timepit.refined.auto._

object GsheetsDbLoader extends App with LoggingSupport {
  object cfg {
    def sheetId: SheetId = "1Dg3Mayy-qw2tTjAjFmU4emeYFa_4ebUfIFPNRQxQpV8"
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = {
    for {
      _ <- logger.infoIO(s"Loading sheet ${cfg.sheetId}...")
      db <- GsheetsTestHelpers.initDb(cfg.sheetId)
      data <- db.load
      _ <- logger.infoIO("")
      _ <- logger.infoIO(s"  $data")
    } yield 0
  }.provideSomeLayer[ZEnv](CacheModule.live).orDie
}
