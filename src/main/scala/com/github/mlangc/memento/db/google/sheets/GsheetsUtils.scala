package com.github.mlangc.memento.db.google.sheets

import java.util
import java.util.Collections

import cats.syntax.option._
import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.slf4zio.api._
import com.google.api.client.googleapis.json.GoogleJsonResponseException
import com.google.api.client.http.HttpResponseException
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.model.BatchUpdateSpreadsheetRequest
import com.google.api.services.sheets.v4.model.CreateDeveloperMetadataRequest
import com.google.api.services.sheets.v4.model.DataFilter
import com.google.api.services.sheets.v4.model.DeleteDeveloperMetadataRequest
import com.google.api.services.sheets.v4.model.DeveloperMetadata
import com.google.api.services.sheets.v4.model.DeveloperMetadataLocation
import com.google.api.services.sheets.v4.model.DeveloperMetadataLookup
import com.google.api.services.sheets.v4.model.Request
import com.google.api.services.sheets.v4.model.UpdateDeveloperMetadataRequest
import com.google.api.services.sheets.v4.model.ValueRange
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import zio.RIO
import zio.Task
import zio.ZIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._

object GsheetsUtils extends LoggingSupport {

  implicit class ValuesOps(val values: Sheets#Spreadsheets#Values) extends AnyVal {
    // TODO: Require SheetId instead of String
    // TODO: Return Iterable[Iterable[AnyRef]]
    // TODO: Consider type for Range
    def getRawValues(sheetId: String, range: String): RIO[Blocking, Iterable[util.List[AnyRef]]] =
      effectBlocking {
        try {
          Option(values.get(sheetId, range).execute().getValues.asScala)
            .getOrElse(Iterable.empty)
        } catch {
          case gre: GoogleJsonResponseException if gre.getStatusCode != 429 =>
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

    def batchGetRawValues(sheetId: SheetId, ranges: Iterable[String]): RIO[Blocking, Map[String, Iterable[Iterable[AnyRef]]]] =
      effectBlocking {
        values.batchGet(sheetId)
          .setRanges(ranges.toSeq.asJava)
          .setMajorDimension("ROWS")
          .execute()
          .getValueRanges
          .asScala
          .zip(ranges)
          .map { case (valueRange, range) =>
            range ->
              Option(valueRange.getValues).map(_.asScala.map(_.asScala)).getOrElse(Iterable.empty)
          }.toMap
      }

    def batchGetStringValues(sheetId: SheetId, ranges: Iterable[String]): RIO[Blocking, Map[String, Iterable[Iterable[String]]]] =
      batchGetRawValues(sheetId, ranges).map(_.view.mapValues(_.map(_.map(String.valueOf))).toMap)

    def batchGetStringValues(sheetId: SheetId, range1: String, range2: String)
    : RIO[Blocking, (Iterable[Iterable[String]], Iterable[Iterable[String]])] =
      batchGetStringValues(sheetId, Iterable(range1, range2)).flatMap { res =>
        (res.get(range1), res.get(range2)) match {
          case (Some(res1), Some(res2)) => ZIO.succeed(res1 -> res2)
          case _ => ZIO.fail(new RuntimeException(s"Requested values for $range1 and $range2, but only got responses for ${res.keys}"))
        }
      }

    def appendRows(sheetId: SheetId, range: String, rows: Iterable[Iterable[String]]): RIO[Blocking, Unit] =
      effectBlocking {
        values
          .append(sheetId, range, rowsToValueRange(rows))
          .setValueInputOption("USER_ENTERED")
          .execute()
      }.unit

    def appendRow(sheetId: SheetId, range: String)(cells: String*): RIO[Blocking, Unit] =
      effectBlocking {
        values
          .append(sheetId, range, rowToValueRange(cells: _*))
          .setValueInputOption("USER_ENTERED")
          .execute()
      }.unit

  }

  implicit class SpreadsheetsOps(val spreadsheets: Sheets#Spreadsheets) extends AnyVal {
    def getSchemaVersion(sheetId: SheetId): RIO[Blocking, Option[SchemaVersion]] =
      effectBlocking {
        spreadsheets.developerMetadata()
          .get(sheetId, DeveloperMetadataIds.SchemaVersion.value)
          .execute()
          .getMetadataValue
      }.flatMap { versionString =>
        Task(versionString.toInt)
          .flatMap { v =>
            ZIO.fromEither(refineV[SchemaVersionRefinement](v))
              .mapError(new RuntimeException(_))
              .map(_.some)
          }
      }.catchSome {
        case e: HttpResponseException if e.getStatusCode == 404 => ZIO.succeed(None)
      }
  }

  def developerMetadataFor(version: SchemaVersion): DeveloperMetadata = {
    val developerMetadata = new DeveloperMetadata
    val developerMetadataLocation = new DeveloperMetadataLocation
    developerMetadataLocation.setSpreadsheet(true)
    developerMetadata.setLocation(developerMetadataLocation)
    developerMetadata.setMetadataId(DeveloperMetadataIds.SchemaVersion.value)
    developerMetadata.setMetadataKey("SchemaVersion")
    developerMetadata.setMetadataValue(version.toString())
    developerMetadata.setVisibility("DOCUMENT")
    developerMetadata
  }

  def batchUpdateSpreadsheetRequestToDeleteSchemaVersion: BatchUpdateSpreadsheetRequest = {
    val deleteRequest = new DeleteDeveloperMetadataRequest
    deleteRequest.setDataFilter(schemaVersionDataFilter)
    val request = new Request
    request.setDeleteDeveloperMetadata(deleteRequest)
    val batchSheetUpdate = new BatchUpdateSpreadsheetRequest
    batchSheetUpdate.setRequests(Collections.singletonList(request))
    batchSheetUpdate
  }

  def batchUpdateSpreadsheetRequestFor(version: SchemaVersion, update: Boolean): BatchUpdateSpreadsheetRequest = {
    val updateRequest = new BatchUpdateSpreadsheetRequest
    updateRequest.setRequests(Collections.singletonList(requestForDeveloperMetadata(version, update)))
    updateRequest
  }

  def requestForDeveloperMetadata(version: SchemaVersion, update: Boolean) =
    if (!update) {
      val developerMetadataRequest = new CreateDeveloperMetadataRequest
      val developerMetadata = GsheetsUtils.developerMetadataFor(version)
      developerMetadataRequest.setDeveloperMetadata(developerMetadata)
      val request = new Request
      request.setCreateDeveloperMetadata(developerMetadataRequest)
      request
    } else {
      val developerMetadataRequest = new UpdateDeveloperMetadataRequest
      val developerMetadata = GsheetsUtils.developerMetadataFor(version)
      developerMetadataRequest.setDeveloperMetadata(developerMetadata)
      developerMetadataRequest.setFields("*")
      developerMetadataRequest.setDataFilters(Collections.singletonList(schemaVersionDataFilter))
      val request = new Request
      request.setUpdateDeveloperMetadata(developerMetadataRequest)
      request
    }

  def rowToValueRange(cells: String*): ValueRange = {
    val valueRange = new ValueRange
    valueRange.setMajorDimension("ROWS")
    valueRange.setValues(Collections.singletonList((cells: Iterable[AnyRef]).toSeq.asJava))
    valueRange
  }

  def rowsToValueRange(rows: Iterable[Iterable[String]]): ValueRange = {
    val valueRange = new ValueRange
    valueRange.setMajorDimension("ROWS")
    valueRange.setValues {
      rows.map((_: Iterable[AnyRef]).toSeq.asJava).toSeq.asJava
    }
    valueRange
  }

  private def schemaVersionDataFilter = {
    val dataFilter = new DataFilter
    val metadataLookup = new DeveloperMetadataLookup
    metadataLookup.setMetadataId(DeveloperMetadataIds.SchemaVersion.value)
    dataFilter.setDeveloperMetadataLookup(metadataLookup)
    dataFilter
  }
}
