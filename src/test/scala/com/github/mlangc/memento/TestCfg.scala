package com.github.mlangc.memento

import java.io.File

import com.github.mlangc.memento.db.google.sheets.SheetId
import com.github.mlangc.memento.db.google.sheets.SheetIdRefinement
import eu.timepit.refined.refineV
import zio.RIO
import zio.URIO
import zio.ZIO
import zio.system
import zio.system.System


object TestCfg {
  def tokensDir: URIO[System, File] =
    system.property("user.home")
      .someOrFail(new RuntimeException("Cannot obtain HOME directory"))
      .map(new File(_, ".memento/test-tokens"))
      .orDie

  def sheetId(envVarName: String): RIO[System, SheetId] =
    system.env(envVarName).flatMap {
      case None => ZIO.fail(new NoSuchElementException(s"Cannot load $envVarName from environment"))
      case Some(value) => ZIO.fromEither(refineV[SheetIdRefinement](value)).mapError(new IllegalArgumentException(_))
    }
}
