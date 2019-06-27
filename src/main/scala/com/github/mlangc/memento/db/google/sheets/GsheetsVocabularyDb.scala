package com.github.mlangc.memento.db.google.sheets

import java.io.{File, FileInputStream, InputStreamReader}
import java.time.Instant
import java.util
import java.util.Collections

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model._
import com.github.mlangc.slf4zio.api.LoggingSupport
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.{GoogleAuthorizationCodeFlow, GoogleClientSecrets}
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.sheets.v4.{Sheets, SheetsScopes}
import com.google.api.services.sheets.v4.model.ValueRange
import com.softwaremill.tagging.Tagger
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import zio.Task

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try

private[sheets] class GsheetsVocabularyDb private(sheetId: String, sheets: Sheets) extends VocabularyDb with LoggingSupport {
  def load: Task[VocabularyData] = Task {
    val sheetValues = sheets.spreadsheets().values()

    def getRawValues(range: String): util.List[util.List[AnyRef]] = logDebugPerformance(d => s"grabbing values took ${d.toMillis}ms", 20.millis) {
      Option(sheetValues.get(sheetId, range).execute().getValues)
        .getOrElse(util.Collections.emptyList())
    }

    val translationsRange = "Translations!A2:B"
    val translationValues = getRawValues(translationsRange)

    val checksRange = "Checks!A2:E"
    val checksValues = getRawValues(checksRange)

    val synonyms1Range = "Synonyms1!A2:B"
    val synonyms2Range = "Synonyms2!A2:B"

    def cellToStr(cell: AnyRef): Option[String] = {
      Option(cell).map(_.toString.trim)
    }

    def cellToSpelling(cell: AnyRef): Option[Spelling] = {
      cellToStr(cell).flatMap(s => refineV[SpellingRefinement](s).toOption)
    }

    def getSynonymValues(range: String): List[Synonym] = {
      getRawValues(range).asScala.flatMap { row =>
        if (row.size() != 2) None else {
          for {
            left <- cellToSpelling(row.get(0))
            right <- cellToSpelling(row.get(1))
          } yield Synonym(left, right)
        }
      }.toList
    }

    val translations = translationValues.asScala.flatMap { row =>
      if (row.size() != 2) None else {
        for {
          left <- cellToSpelling(row.get(0))
          right <- cellToSpelling(row.get(1))
        } yield Translation(left, right)
      }
    }

    def toInstant(str: String): Option[Instant] = {
      Try(Instant.parse(str)).toOption
    }

    val checks = checksValues.asScala.flatMap { row =>
      if (row.size() != 5) None else {
        for {
          leftWord <- cellToSpelling(row.get(0))
          rightWord <- cellToSpelling(row.get(1))
          timestamp <- cellToStr(row.get(4)).flatMap(toInstant)
          score <- cellToStr(row.get(3)).flatMap(Score.parse)
          direction <- cellToStr(row.get(2)).flatMap(Direction.fromString)
        } yield Check(Translation(Vocabulary(leftWord), Vocabulary(rightWord)), direction, score, timestamp)
      }
    }

    val (language1: LanguageName, language2: LanguageName) = {
      getRawValues("Translations!A1:B1")
        .asScala.flatMap(_.asScala)
        .flatMap(cellToStr) match {
          case Seq(lang1, lang2) => (lang1.taggedWith[LanguageNameTag], lang2.taggedWith[LanguageNameTag])
          case other => throw new AssertionError(s"Expected two results, but got $other")
      }
    }


    VocabularyData(
      language1 = language1,
      language2 = language2,
      translations = translations.toList,
      checks = checks.toList,
      synonyms1 = getSynonymValues(synonyms1Range),
      synonyms2 = getSynonymValues(synonyms2Range))
  }

  def addCheck(check: Check): Task[Unit] = Task {
    val valueRange = new ValueRange
    valueRange.setMajorDimension("ROWS")

    valueRange.setValues {
      Collections.singletonList {
        util.Arrays.asList(
          check.translation.left.spelling.value,
          check.translation.right.spelling.value,
          check.direction.toString,
          check.score.toString,
          check.timestamp.toString,
        )
      }
    }

    logDebugPerformance(d => s"Updating sheet took ${d.toMillis}ms", 10.millis) {
      sheets.spreadsheets().values()
        .append(sheetId, "Checks!A2:E", valueRange)
        .setValueInputOption("USER_ENTERED")
        .execute()
    }
  }.unit
}

object GsheetsVocabularyDb {
  private lazy val transport = GoogleNetHttpTransport.newTrustedTransport()
  private lazy val jsonFactory = JacksonFactory.getDefaultInstance

  def make(sheetId: String, secrets: File): Task[GsheetsVocabularyDb] = Task {
    val secretsIn = new FileInputStream(secrets)
    try {
      val clientSecrets = GoogleClientSecrets.load(jsonFactory, new InputStreamReader(secretsIn))
      val scopes = Collections.singletonList(SheetsScopes.SPREADSHEETS)

      val flow = new GoogleAuthorizationCodeFlow.Builder(transport, jsonFactory, clientSecrets, scopes)
        .setDataStoreFactory(new FileDataStoreFactory(new File("tokens")))
        .setAccessType("offline")
        .build()

      val receiver = new LocalServerReceiver.Builder().setPort(8888).build()
      val credentials = new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
      val service = new Sheets.Builder(transport, jsonFactory, credentials)
        .setApplicationName("memento")
        .build()

      new GsheetsVocabularyDb(sheetId, service)

    } finally {
      secretsIn.close()
    }
  }
}

