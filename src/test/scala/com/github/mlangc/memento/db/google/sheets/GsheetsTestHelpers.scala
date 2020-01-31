package com.github.mlangc.memento.db.google.sheets

import java.io.File

import cats.syntax.option._
import com.github.ghik.silencer.silent
import com.github.mlangc.memento.db.google.Gauthorizer
import com.github.mlangc.memento.db.google.drive.GdriveService
import com.github.mlangc.memento.util.zio.ZioUtils
import com.github.mlangc.slf4zio.api._
import com.google.api.services.drive.Drive
import com.google.api.services.drive.model
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.SheetsScopes
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import zio.RIO
import zio.UIO
import zio.ZManaged
import zio.blocking.Blocking
import zio.blocking.effectBlocking


private[sheets] object GsheetsTestHelpers extends LoggingSupport {
  @volatile
  private var cache: Option[(Sheets, Drive)] = None

  def copy(id: SheetId): RIO[Blocking, SheetId] =
    effectWithDrive { drive =>
      val file = drive.files().copy(id, new model.File()).execute()
      refineV[SheetIdRefinement](file.getId)
        .getOrElse(throw new RuntimeException(file.getId))
    }

  def tmpCopy(id: SheetId): ZManaged[Blocking, Throwable, SheetId] =
    copy(id).toManaged(id => delete(id).catchAll(e => logger.errorIO(s"Error deleting sheet $id", e)))

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

  @silent("never used")
  private def effectWithSheets[A](effect: Sheets => A): RIO[Blocking, A] =
    sheetsAndDrive.flatMap { case (sheets, _) => effectBlocking(effect(sheets)) }

  @silent("never used")
  private def effectWithSheetsAndDrive[A](effect: (Sheets, Drive) => A): RIO[Blocking, A] = {
    sheetsAndDrive.flatMap(args => effectBlocking(effect.tupled(args)))
  }
}
