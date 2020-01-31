package com.github.mlangc.memento.db.google.sheets

import com.github.ghik.silencer.silent
import com.github.mlangc.memento.TestCfg
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import eu.timepit.refined.auto._
import zio.Managed
import zio.Task
import zio.ZManaged
import zio.blocking.Blocking
import zio.system.System
import GsheetsUtils.ValuesOps


@silent("inferred to be `Any`")
class GsheetsVocabularyDbTest extends GenericVocabularyDbTest {
  "Verify that data is loaded correctly" inIO {
    db.use { db =>
      db.load.flatMap { data =>
        Task {
          assert(data.translations.contains(Translation("ich", "je")))
          assert(data.translations.contains(Translation("sie", "elle")))
          assert(data.checks.filter(_.translation == Translation("test", "test")).nonEmpty)
          assert(data.synonyms1.contains(Synonym("dumm", "blöd")))
          assert(data.synonyms1.contains(Synonym("in Hinblick auf", "in Bezug auf")))
          assert(data.synonyms2.contains(Synonym("forêt", "bois")))
          assert(data.language1.value === "Deutsch")
          assert(data.language2.value === "Französisch")
        }
      }
    }
  }

  "Verify that an old sheet is migrated correctly" inIO {
    val tmpCopy: ZManaged[Blocking, Throwable, SheetId] =
        GsheetsTestHelpers.tmpCopy(TestSheetIds.NeedsMigration)

    tmpCopy.use { sheetId =>
      for {
        db1 <- initDb(sheetId)
        v1 <- db1.load
        db2 <- initDb(sheetId)
        v2 <- db2.load
        tokensDir <- TestCfg.tokensDir
        sheets <- GsheetsService.make(tokensDir)
        values <- Task(sheets.spreadsheets().values())
        v3 <- values.getStringValues(sheetId, "Checks!F1:F2")
        _ <- Task(assert(v1 == v2 && v3 == List("", "8719ec03b2b3fd089e7db06f40d9f1ec8aba0293")))
      } yield ()
    }
  }

  protected def db: Managed[Throwable, VocabularyDb] =
    initDb(TestSheetIds.Simple).toManaged_

  private def initDb(sheetId: SheetId): Task[GsheetsVocabularyDb] =
    TestCfg.tokensDir.flatMap { case tokensDir =>
      GsheetsVocabularyDb.make(sheetId, tokensDir)
    }.provide(new Blocking.Live with System.Live {})
}
