package com.github.mlangc.memento.db.google.sheets

import com.github.ghik.silencer.silent
import com.github.mlangc.memento.TestCfg
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.ValuesOps
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.zenvs.ZLayers
import eu.timepit.refined.auto._
import zio.Managed
import zio.RIO
import zio.Task
import zio.ZManaged
import zio.blocking.Blocking
import zio.clock.Clock
import zio.system.System


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

  "Verify that old sheets are migrated correctly" - {
    def checkMigration(sheetId: SheetId, expectedShas: String*): RIO[Blocking with Clock with System, Unit] = {
      val tmpCopy: ZManaged[Blocking with Clock, Throwable, SheetId] =
        GsheetsTestHelpers.tmpCopy(sheetId)

      tmpCopy.use { sheetId =>
        for {
          db1 <- initDb(sheetId)
          v1 <- db1.load
          db2 <- initDb(sheetId)
          v2 <- db2.load
          tokensDir <- TestCfg.tokensDir
          sheets <- GsheetsService.make(tokensDir)
          values <- Task(sheets.spreadsheets().values())
          v3 <- values.getStringValues(sheetId, "Checks!G1:G")
          _ <- Task(assert(v1 == v2 && v3.flatten == ("Sha1-1000" +: expectedShas)))
        } yield ()
      }
    }

    "with a small test sheet" inIO {
      checkMigration(TestSheetIds.NeedsMigrationSmall, "a71711533217ed8707fd6b5f88c4ac472a15e0f8")
    }

    "with a big test sheet" inIO {
      checkMigration(TestSheetIds.NeedsMigrationBig,
        "2757cb3acfd52ba6b41e25686de3906791d23204",
        "24c2302bac971e05be395094a87e18cc24ae4779",
        "d8a619f2e20e0e009e04108427261309d0a4ea82",
        "8a7ebcb9754a37942fbb71584d365456d8fdb2a5",
        "a1ef07f1b71bbcadfccc131f379cef27326a0f66",
        "7ed43792ed1801ee9a28c9dd5ee5e8ddd46ff963",
      )
    }
  }

  protected def db: Managed[Throwable, VocabularyDb] =
    initDb(TestSheetIds.Simple).toManaged_

  private def initDb(sheetId: SheetId): Task[GsheetsVocabularyDb] =
    GsheetsTestHelpers.initDb(sheetId).provideLayer(ZLayers.live)
}
