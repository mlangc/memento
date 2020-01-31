package com.github.mlangc.memento.db.google.sheets

import java.util

import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.slf4zio.api._
import com.google.api.client.googleapis.json.GoogleJsonResponseException
import com.google.api.services.sheets.v4.Sheets
import eu.timepit.refined.auto._
import zio.RIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

object GsheetsUtils extends LoggingSupport {
  implicit class ValuesOps(val values: Sheets#Spreadsheets#Values) extends AnyVal {
    def getRawValues(sheetId: String, range: String): RIO[Blocking, Iterable[util.List[AnyRef]]] = effectBlocking {
      try {
        Option(values.get(sheetId, range).execute().getValues.asScala)
          .getOrElse(Iterable.empty)
      } catch {
        case gre: GoogleJsonResponseException =>
          val checkMsg = {
            if (gre.getDetails.getCode == 404) "Please check your configuration"
            else "Please verify that your sheet is properly structured"
          }

          val errTitle = s"""Error loading range "$range" from sheet $sheetId"""
          val errMsg = s"${gre.getStatusCode} - ${gre.getStatusMessage}"

          throw new ErrorMessage(s"$errTitle\n$errMsg\n$checkMsg", gre)
      }
    }.logDebugPerformance(d => s"grabbing values for '$range' took ${d.toMillis}ms", 20.millis)

    def getStringValues(sheetId: SheetId, range: String): RIO[Blocking, Iterable[Iterable[String]]] =
      getRawValues(sheetId, range).map(_.map(_.asScala.map(String.valueOf)))
  }
}
