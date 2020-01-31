package com.github.mlangc.memento.db.google.sheets

import java.io.File
import java.io.FileNotFoundException
import java.time.Instant
import java.util
import java.util.Collections

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.ValuesOps
import com.github.mlangc.memento.db.model._
import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.slf4zio.api.LoggingSupport
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.model.ValueRange
import eu.timepit.refined.auto._
import eu.timepit.refined.refineV
import zio.RIO
import zio.Task
import zio.ZIO
import zio.blocking.Blocking

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

private[sheets] class GsheetsVocabularyDb private(sheetId: String,
                                                  sheets: Sheets,
                                                  blockingModule: Blocking)
  extends VocabularyDb with LoggingSupport {

  import blockingModule.blocking.effectBlocking

  def load: Task[VocabularyData] = {
    for {
      sheetValues <- Task(sheets.spreadsheets().values())
      getRawValues = (range: String) => sheetValues.getRawValues(sheetId, range).provide(blockingModule)
      data <- {
        def cellToStr(cell: AnyRef): Option[String] = {
          Option(cell).map(_.toString.trim)
        }

        def cellToSpelling(cell: AnyRef): Option[Spelling] = {
          cellToStr(cell).flatMap(s => refineV[SpellingRefinement](s).toOption)
        }

        def getSynonymValues(range: String): Task[List[Synonym]] = getRawValues(range).map { rawValues =>
          rawValues.flatMap { row =>
            if (row.size() != 2) None else {
              for {
                left <- cellToSpelling(row.get(0))
                right <- cellToSpelling(row.get(1))
              } yield Synonym(left, right)
            }
          }.toList
        }

        val langNames: Task[(LanguageName, LanguageName)] = {
          getRawValues("Translations!A1:B1").map { rawValues =>
            rawValues.flatMap(_.asScala).flatMap(cellToStr).toSeq match {
              case Seq(lang1: String, lang2: String) =>
                (refineV[LanguageNameRefinement](lang1), refineV[LanguageNameRefinement](lang2)) match {
                  case (Right(lang1), Right(lang2)) => (lang1, lang2)

                  case (Left(error), Right(_)) =>
                    throw new IllegalStateException(s"Expected a language name but got '$lang1': $error")

                  case (Right(_), Left(error)) =>
                    throw new IllegalStateException(s"Expected a language name but got '$lang2': $error")

                  case (Left(error1), Left(error2)) =>
                    throw new IllegalStateException(s"Expected language names, but '$lang1' (error: $error1) and '$lang2' (error: $error2)")
                }
              case other => throw new IllegalStateException(s"Expected two results, but got $other")
            }
          }
        }

        val translationsRange = "Translations!A2:B"
        val translationValues = getRawValues(translationsRange)

        val checksRange = "Checks!A2:E"
        val checksValues = getRawValues(checksRange)

        def synonmRanges(langNames: (LanguageName, LanguageName)): (String, String) = langNames match {
          case (language1, language2) =>
            val synonyms1Range = s"'Synonyms $language1'!A2:B"
            val synonyms2Range = s"'Synonyms $language2'!A2:B"
            (synonyms1Range, synonyms2Range)
        }

        val translations: Task[Iterable[Translation]] = translationValues.map { translationValues =>
          translationValues.flatMap { row =>
            if (row.size() != 2) None else {
              for {
                left <- cellToSpelling(row.get(0))
                right <- cellToSpelling(row.get(1))
              } yield Translation(left, right)
            }
          }
        }

        def toInstant(str: String): Option[Instant] = {
          Try(Instant.parse(str)).toOption
        }

        val checks: Task[Iterable[Check]] = checksValues.map { checksValues =>
          checksValues.flatMap { row =>
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
        }

        for {
          langNamesChecksTranslations <- zip3Par(langNames, checks, translations)
          synRanges = synonmRanges(langNamesChecksTranslations._1)
          synonyms <- getSynonymValues(synRanges._1).zipPar(getSynonymValues(synRanges._2))
        } yield {
          VocabularyData(
            language1 = langNamesChecksTranslations._1._1,
            language2 = langNamesChecksTranslations._1._2,
            translations = langNamesChecksTranslations._3.toList,
            checks = langNamesChecksTranslations._2.toList,
            synonyms1 = synonyms._1,
            synonyms2 = synonyms._2)
        }
      }
    } yield data
    }.logDebugPerformance(d => s"Loading sheet took ${d.toMillis}ms", 100.millis)

  def addCheck(check: Check): Task[Unit] = effectBlocking {
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

  private def zip3Par[A, B, C](a: Task[A], b: Task[B], c: Task[C]): Task[(A, B, C)] =
    a.zipPar(b).zipWithPar(c) { case ((a, b), c) => (a, b, c) }
}

object GsheetsVocabularyDb {
  def make(cfg: GsheetsCfg): RIO[Blocking, GsheetsVocabularyDb] = make(cfg.sheetId, new File(cfg.tokensPath))

  def make(sheetId: String, tokensDir: File): RIO[Blocking, GsheetsVocabularyDb] =
    GsheetsService.make(tokensDir).flatMap { service =>
      ZIO.access[Blocking](blocking => new GsheetsVocabularyDb(sheetId, service, blocking))
    }.mapError {
      case fnf: FileNotFoundException =>
        new ErrorMessage(s"Please verify your configuration:\n  ${fnf.getMessage}", fnf)

      case e => e
    }
}

