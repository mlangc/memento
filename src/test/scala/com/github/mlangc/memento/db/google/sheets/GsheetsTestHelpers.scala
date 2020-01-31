package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.db.google.drive.GdriveService
import com.github.mlangc.memento.util.zio.ZioUtils
import com.google.api.services.drive.Drive
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.SheetsScopes
import eu.timepit.refined.auto._
import zio.RIO
import zio.UIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking
import eu.timepit.refined.refineV
import cats.syntax.option._
import com.github.mlangc.memento.db.google.Gauthorizer


private[sheets] object GsheetsTestHelpers {
  @volatile
  private var cache: Option[(Sheets, Drive)] = None

  def copy(id: SheetId): RIO[Blocking, SheetId] =
    effectWithSheets { sheets =>
      val spreadsheets = sheets.spreadsheets()
      val src = spreadsheets.get(id).setIncludeGridData(true).execute().setSpreadsheetId(null)
      val template = spreadsheets.create(src).execute()

      refineV[SheetIdRefinement](template.getSpreadsheetId)
        .getOrElse(throw new RuntimeException(template.getSpreadsheetId))
    }

  def delete(id: SheetId): RIO[Blocking, Unit] =
    effectWithDrive(_.files().delete(id).execute()).unit

  private val sheetsAndDrive: RIO[Blocking, (Sheets, Drive)] = {
    val loadSheets: RIO[Blocking, (Sheets, Drive)] =
      GsheetsCfg.loadDefaultTokensPath.flatMap { path =>
        val dir = new File(path)
        Gauthorizer.authorize(dir, SheetsScopes.SPREADSHEETS, SheetsScopes.DRIVE).flatMap { credential =>
          GsheetsService.make(dir, credential.some).zipPar(GdriveService.make(dir, credential.some))
        }
      }

    def set(arg: (Sheets, Drive)) = UIO { cache = Some(arg); () }
    val get = UIO(cache)

    ZioUtils.cached(loadSheets)(get, set)
  }

  private def effectWithDrive[A](effect: Drive => A): RIO[Blocking, A] =
    sheetsAndDrive.flatMap { case (_, drive) => effectBlocking(effect(drive)) }

  private def effectWithSheets[A](effect: Sheets => A): RIO[Blocking, A] =
    sheetsAndDrive.flatMap { case (sheets, _) => effectBlocking(effect(sheets)) }
}
