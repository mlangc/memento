package com.github.mlangc.memento.db.google.sheets

import eu.timepit.refined.auto._
import zio.App
import zio.ZIO
import GsheetsUtils.ValuesOps
import com.github.mlangc.slf4zio.api._

object GsheetsRangePrinter extends App with LoggingSupport {
  object cfg {
    val ranges = List("Checks!G2:G", "Checks!F2:G")
    val sheetId: SheetId = "1n32I_6ozkQ9kV7yGfLiXWdMN7gvfiMzwmi0PMENpBvY"
  }

  def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    logger.infoIO(s"Grabbing ${cfg.ranges} from ${cfg.sheetId}...") *>
      GsheetsTestHelpers.withSheets { sheets =>
          sheets.spreadsheets().values().batchGetStringValues(cfg.sheetId, cfg.ranges)
        }.perfLog(LogSpec.onSucceed(d => debug"Grabbing values took ${d.toMillis}ms")).flatMap { res =>
        logger.infoIO("Done - printing results row by row:") *>
          ZIO.foreach_(cfg.ranges) { range =>
            logger.infoIO(s"  Range '$range':") *>
              ZIO.foreach(res.get(range).toList.flatten)(row => logger.infoIO(s"    $row"))
          }
      }.as(0).orDie
}
