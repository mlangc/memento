package com.github.mlangc.memento.db.google.sheets

import java.io.File
import java.io.FileNotFoundException
import java.time.Instant

import com.github.ghik.silencer.silent
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.cache.BasicCachables.checksCachable
import com.github.mlangc.memento.db.cache.CacheModule
import com.github.mlangc.memento.db.google.RetrySchedules
import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.SpreadsheetsOps
import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.ValuesOps
import com.github.mlangc.memento.db.model._
import com.github.mlangc.memento.errors.ErrorMessage
import com.github.mlangc.memento.util.zio.RefinementError
import com.github.mlangc.memento.util.zio.ZioUtils
import com.github.mlangc.slf4zio.api._
import com.google.api.services.sheets.v4.Sheets
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosInt
import zio.RIO
import zio.Schedule
import zio.Task
import zio.ZIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking
import zio.clock.Clock

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

private[sheets] class GsheetsVocabularyDb private(sheetId: SheetId,
                                                  @silent("never used") cacheDir: File,
                                                  sheets: Sheets,
                                                  modules: Blocking with Clock with CacheModule)
  extends VocabularyDb with LoggingSupport {

  private def hashStride: PosInt = 1000
  private val hashFormula = tableHashFormula("A", "E", hashStride, 1)
  private val defaultRetrySchedule = RetrySchedules.gapiCall()

  def load: Task[VocabularyData] =
    modules.makeSimpleCache(cacheDir).use { simpleCache =>
      for {
        spreadsheets <- Task(sheets.spreadsheets())
        sheetValues <- Task(spreadsheets.values())
        getRawValues = (range: String) => sheetValues.getRawValues(sheetId, range).retry(defaultRetrySchedule)
        batchGetStringValues = (range1: String, range2: String) => sheetValues.batchGetStringValues(sheetId, range1, range2).retry(defaultRetrySchedule)
        appendRows = (range: String, rows: Iterable[Iterable[String]]) => sheetValues.appendRows(sheetId, range, rows).retry(defaultRetrySchedule)
        getStringValues = (range: String) => sheetValues.getStringValues(sheetId, range).retry(defaultRetrySchedule)
        data <- {
          def cellToStr(cell: AnyRef): Option[String] = {
            Option(cell).map(_.toString.trim)
          }

          def cellToSpelling(cell: AnyRef): Option[Spelling] = {
            cellToStr(cell).flatMap(s => refineV[SpellingRefinement](s).toOption)
          }

          def toInstant(str: String): Option[Instant] = {
            Try(Instant.parse(str)).toOption
          }

          def getSynonymValues(range: String): RIO[Clock with Blocking, List[Synonym]] = getRawValues(range).map { rawValues =>
            rawValues.flatMap { row =>
              if (row.size() != 2) None else {
                for {
                  left <- cellToSpelling(row.get(0))
                  right <- cellToSpelling(row.get(1))
                } yield Synonym(left, right)
              }
            }.toList
          }

          val langNames: RIO[Clock with Blocking, (LanguageName, LanguageName)] = {
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

          val lastRowRange = "Checks!F2:F2"
          val shasRange = "Checks!G2:G"
          val checksCacheKeys: RIO[Blocking with Clock, List[HashedRangeCacheKey]] = {
            def shouldRetry(th: Throwable) = th match {
              case e@RefinementError(v: String, _) if v == "#NAME?" || v.endsWith("...") =>
                logger.warnIO(s"Retrying to load hashes after error: $e").as(true)

              case _ => ZIO.succeed(false)
            }

            val schedule = Schedule.doWhileM(shouldRetry)

            batchGetStringValues(lastRowRange, shasRange).flatMap { case (lastRowRes, shasRes) =>
              val lastRow =
                for {
                  str <- ZIO.fromOption(lastRowRes.headOption.flatMap(_.headOption)).orElse(ZIO.fail(new IllegalStateException("Got empty result")))
                  num <- Task(str.toInt)
                  last <- ZioUtils.refineIO[Positive](num)
                } yield last

              val numCacheKeysExpected: RIO[Blocking, Int] =
                lastRow.map(lastRow => ((lastRow - 1).toDouble / hashStride).ceil.toInt)

              numCacheKeysExpected.flatMap { numCacheKeysExpected =>
                def toShas(res: Iterable[Iterable[String]]) =
                  ZIO.foreach(res.flatten)(hash => ZioUtils.refineIO[Sha1Refinement](hash))

                toShas(shasRes).flatMap { shas =>
                  val shasMissing = numCacheKeysExpected - shas.size
                  if (shasMissing <= 0) ZIO.succeed(shas) else {
                    appendRows(shasRange, Iterable.fill(shasMissing)(Iterable.single(hashFormula))) *> {
                      val startRow = 2 + shas.size
                      val shasRange2 = s"Checks!G${startRow}:G${startRow + shasMissing - 1}"
                      getStringValues(shasRange2)
                        .flatMap(toShas)
                        .map(shas ++ _)
                    }
                  }
                }
              }
            }.map(_.zipWithIndex.map((HashedRangeCacheKey.apply _).tupled))
              .retry(schedule)
          }


          val translationsRange = "Translations!A2:B"
          val translationValues = getRawValues(translationsRange)

          val checks: RIO[Blocking with Clock, List[Check]] =
            checksCacheKeys.flatMap { cacheKeys =>
              ZIO.foreach(cacheKeys) { cacheKey =>
                simpleCache.load(cacheKey) { cacheKey =>
                  val range = cacheKey.range("Checks", "A", "E", hashStride, 1)
                  val checksValues = getRawValues(range)
                  val checks: RIO[Clock with Blocking, Iterable[Check]] = checksValues.map { checksValues =>
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

                  checks
                }
              }
            }.map(_.flatten)

          def synonymRanges(langNames: (LanguageName, LanguageName)): (String, String) = langNames match {
            case (language1, language2) =>
              val synonyms1Range = s"'Synonyms $language1'!A2:B"
              val synonyms2Range = s"'Synonyms $language2'!A2:B"
              (synonyms1Range, synonyms2Range)
          }

          val translations: RIO[Clock with Blocking, Iterable[Translation]] = translationValues.map { translationValues =>
            translationValues.flatMap { row =>
              if (row.size() != 2) None else {
                for {
                  left <- cellToSpelling(row.get(0))
                  right <- cellToSpelling(row.get(1))
                } yield Translation(left, right)
              }
            }
          }

          def eventuallyMigrate(version: SchemaVersion): RIO[Clock with Blocking, Unit] =
            ZIO.when(version != SchemaVersions.Current) {
              if (version != SchemaVersions.V0) {
                ZIO.fail(new RuntimeException(s"Cannot migrate sheet $sheetId from schema $version to ${SchemaVersions.Current}"))
              } else {
                val rows = Iterable(
                  Iterable("Last Row", s"Sha1-$hashStride"),
                  Iterable(lastRowFormula, "")
                )
                appendRows("Checks!F1:G", rows)
              } *> effectBlocking {
                spreadsheets
                  .batchUpdate(sheetId, GsheetsUtils.batchUpdateSpreadsheetRequestFor(SchemaVersions.Current, false))
                  .execute()
              }.retry(defaultRetrySchedule)
            }

          for {
            schemaVersion <- spreadsheets.getSchemaVersion(sheetId).map(_.getOrElse(SchemaVersions.V0))
            _ <- logger.debugIO(s"Sheet $sheetId has schema version $schemaVersion")
            _ <- eventuallyMigrate(schemaVersion)
            langNamesChecksTranslations <- zip3Par(langNames, checks, translations)
            synRanges = synonymRanges(langNamesChecksTranslations._1)
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
    }.logDebugPerformance(d => s"Loading sheet took ${d.toMillis}ms", 100.millis).provide(modules)

  def addCheck(check: Check): Task[Unit] =
    sheets.spreadsheets().values().appendRow(sheetId, "Checks!A2:F")(
      check.translation.left.spelling.value,
      check.translation.right.spelling.value,
      check.direction.toString,
      check.score.toString,
      check.timestamp.toString,
      hashFormula
    ).logDebugPerformance(d => s"Updating sheet took ${d.toMillis}ms", 10.millis)
      .provide(modules)


  private def zip3Par[R, A, B, C](a: RIO[R, A], b: RIO[R, B], c: RIO[R, C]): RIO[R, (A, B, C)] =
    a.zipPar(b).zipWithPar(c) { case ((a, b), c) => (a, b, c) }

  private def tableHashFormula(left: String, right: String, stride: Refined[Int, Positive], offset: Refined[Int, Positive]) =
    s"""=tableHash(indirect(sliceFor(row(), "$left", "$right", $stride, $offset)))"""

  private def lastRowFormula =
    """=SUMPRODUCT(MAX((A2:A<>"")*ROW(A2:A)))"""
}

object GsheetsVocabularyDb {
  def make(cfg: GsheetsCfg): RIO[CacheModule with Blocking with Clock, GsheetsVocabularyDb] =
    make(cfg.sheetId, new File(cfg.tokensPath), new File(cfg.cachePath))

  def make(sheetId: SheetId, tokensDir: File, cachePath: File): RIO[CacheModule with Blocking with Clock, GsheetsVocabularyDb] =
    GsheetsService.make(tokensDir).flatMap { service =>
      ZIO.access[Blocking with CacheModule with Clock](modules => new GsheetsVocabularyDb(sheetId, cacheDirFor(cachePath, sheetId), service, modules))
    }.mapError {
      case fnf: FileNotFoundException =>
        new ErrorMessage(s"Please verify your configuration:\n  ${fnf.getMessage}", fnf)

      case e => e
    }

  private def cacheDirFor(cachePath: File, id: SheetId): File =
    new File(new File(cachePath, "sheets"), id)
}

