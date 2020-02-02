package com.github.mlangc.memento.db.google.sheets

import java.util
import java.util.Collections

import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.slf4zio.api._
import com.google.api.client.googleapis.json.GoogleJsonResponseException
import com.google.api.client.http.HttpResponseException
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.model.BatchUpdateSpreadsheetRequest
import com.google.api.services.sheets.v4.model.CreateDeveloperMetadataRequest
import com.google.api.services.sheets.v4.model.DataFilter
import com.google.api.services.sheets.v4.model.DeveloperMetadata
import com.google.api.services.sheets.v4.model.DeveloperMetadataLocation
import com.google.api.services.sheets.v4.model.DeveloperMetadataLookup
import com.google.api.services.sheets.v4.model.Request
import com.google.api.services.sheets.v4.model.UpdateDeveloperMetadataRequest
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import zio.RIO
import zio.Task
import zio.ZIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import cats.syntax.option._
import com.google.api.services.sheets.v4.model.DeleteDeveloperMetadataRequest

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

  def batchUpdateSpreadsheetRequestFor(version: SchemaVersion, update: Boolean): BatchUpdateSpreadsheetRequest =
    if (!update) {
      val developerMetadataRequest = new CreateDeveloperMetadataRequest
      val developerMetadata = GsheetsUtils.developerMetadataFor(version)
      developerMetadataRequest.setDeveloperMetadata(developerMetadata)
      val batchSheetUpdate = new BatchUpdateSpreadsheetRequest
      val request = new Request
      request.setCreateDeveloperMetadata(developerMetadataRequest)
      batchSheetUpdate.setRequests(Collections.singletonList(request))
      batchSheetUpdate
    } else {
      val developerMetadataRequest = new UpdateDeveloperMetadataRequest
      val developerMetadata = GsheetsUtils.developerMetadataFor(version)
      developerMetadataRequest.setDeveloperMetadata(developerMetadata)
      developerMetadataRequest.setFields("*")
      developerMetadataRequest.setDataFilters(Collections.singletonList(schemaVersionDataFilter))
      val batchSheetUpdate = new BatchUpdateSpreadsheetRequest
      val request = new Request
      request.setUpdateDeveloperMetadata(developerMetadataRequest)
      batchSheetUpdate.setRequests(Collections.singletonList(request))
      batchSheetUpdate
    }

  private def schemaVersionDataFilter = {
    val dataFilter = new DataFilter
    val metadataLookup = new DeveloperMetadataLookup
    metadataLookup.setMetadataId(DeveloperMetadataIds.SchemaVersion.value)
    dataFilter.setDeveloperMetadataLookup(metadataLookup)
    dataFilter
  }
}
