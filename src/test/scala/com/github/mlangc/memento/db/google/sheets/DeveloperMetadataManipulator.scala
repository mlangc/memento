package com.github.mlangc.memento.db.google.sheets

import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.SpreadsheetsOps
import com.github.mlangc.slf4zio.api._
import eu.timepit.refined.auto._
import zio.App
import zio.RIO
import zio.ZEnv
import zio.ZIO
import zio.blocking.Blocking

object DeveloperMetadataManipulator extends App with LoggingSupport {

  object cfg {
    val targetSheetId: SheetId = "1Dg3Mayy-qw2tTjAjFmU4emeYFa_4ebUfIFPNRQxQpV8"
    val schemaVersionToSet: Option[SchemaVersion] = None
  }

  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    GsheetsTestHelpers.withSheets { sheets =>
      sheets.spreadsheets().getSchemaVersion(cfg.targetSheetId)
    }.flatMap { schemaVersion =>
      logger.infoIO(s"Sheet ${cfg.targetSheetId} currently has schema version $schemaVersion") *>
        eventuallySetOrDeleteSchemaVersion(schemaVersion)
    }.catchAll { e =>
      logger.errorIO(s"Error manipulating DeveloperMetadata for sheet ${cfg.targetSheetId}", e).as(1)
    }.as(0)

  private def eventuallySetOrDeleteSchemaVersion(current: Option[SchemaVersion]): RIO[Blocking, Unit] =
    cfg.schemaVersionToSet match {
      case None if current.isEmpty =>
        logger.infoIO("Schema version already deleted; nothing to do")

      case None =>
        GsheetsTestHelpers.effectWithSheets { sheets =>
          val batchUpdate = GsheetsUtils.batchUpdateSpreadsheetRequestToDeleteSchemaVersion
          sheets.spreadsheets().batchUpdate(cfg.targetSheetId, batchUpdate).execute()
        } *> logger.infoIO("Schema version successfully deleted")

      case Some(version) if current.contains(version) =>
        logger.infoIO("Version already set to desired value; nothing to do")

      case Some(version) =>
        GsheetsTestHelpers.effectWithSheets { sheets =>
          val update = current.nonEmpty
          val batchUpdate = GsheetsUtils.batchUpdateSpreadsheetRequestFor(version, update)
          sheets.spreadsheets().batchUpdate(cfg.targetSheetId, batchUpdate).execute()
        } *> logger.infoIO("Version update succeeded")
    }
}
