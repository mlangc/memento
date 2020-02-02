package com.github.mlangc.memento.db.google.sheets

import com.github.ghik.silencer.silent
import com.github.mlangc.memento.TestCfg
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.google.sheets.GsheetsUtils.ValuesOps
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.zenvs.ZEnvs
import eu.timepit.refined.auto._
import zio.Managed
import zio.RIO
import zio.Task
import zio.ZManaged
import zio.blocking.Blocking
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
    def checkMigration(sheetId: SheetId, expectedShas: String*): RIO[Blocking with System, Unit] = {
      val tmpCopy: ZManaged[Blocking, Throwable, SheetId] =
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
          v3 <- values.getStringValues(sheetId, "Checks!F1:F")
          _ <- Task(assert(v1 == v2 && v3.flatten == ("Sha1-100" +: expectedShas)))
        } yield ()
      }
    }

    "with a small test sheet" inIO {
      checkMigration(TestSheetIds.NeedsMigrationSmall, "8719ec03b2b3fd089e7db06f40d9f1ec8aba0293")
    }

    "with a big test sheet" inIO {
      checkMigration(TestSheetIds.NeedsMigrationBig,
        "4cc19358545db7928bf1fbecbdca57cb50f82754",
        "07412286aef839d44ebb4493912ba0d0448644f8",
        "a525bd6f54482066c881651f37552a1812750b45",
        "588d235f5037c1df6c3c477e741d8dbd5702e975",
        "86c7bfbfef70844ac76facb6bedfb8fbbcec031c",
        "c0ebbe4bc6ac40dfbb96760944d117e09b718f83",
        "adeb6191d6d44fee70c7c62476e3e83b0ee18f63",
        "4a621620485ea94e27165db7f16804e1fa553151",
        "ec17390e88eb0507ea0b0ae7090f91e1a7dbadde",
        "949b8884686dae4961498b002392c5880369ea1f",
        "fbe60e42adddea8382e7e4cac5eabe0516acd4c3",
        "e694f41cc23e3e48c633d7d25ab764cec16c3d9f",
        "e3ea152c5a849daecd514212dfefa7e789fce546",
        "21339d0d24c66fc7c159ed17dd9f3ceee37cc66e",
        "7d585e932549e044075b4c67129f9b0937a0f5a6",
        "7c9cefba4e4764076999cbc826a0841112ff4f52",
        "5f7440de92ac9a009ed8b6dd7a1a5c8a27d7acdf",
        "9d5fb102ced38cb95d222a3bb637776af11cf5e6",
        "b5a622ac247e44c42db4a04629d57027539a57e8",
        "56886add88aed2911e181f7f84a29606600aa6d7",
        "e7f646cb16dcb0db4bd25259300131934503d919",
        "3e6e07e485f743aada1583989f36c090bdd167ea",
        "79f87dcfebad57beebd013f17613d5b7ff224181",
        "9af9a38d7bd08b0507ce9f9e91b265b8667d8cc3",
        "0844db98dbf7b433799a7cf4b7214c4ad8970867",
        "8f6425733158d4ad094ba0e649616332e0d8ec3e",
        "50b8ea5f592057fc1747c4e4a523722df7416336",
        "7718ffb1077c788e7c8c6863d4bea875f894285e",
        "06ad903dd9af93a9eb7b60efb2ea3348f60314ae",
        "afe5c37d036340a2a01c26456ffb666cc37c3070",
        "42f0cd45251704a4cbe8a738cd7b2179057e0dd7",
        "5546c87a23aa0dcd5e31ecc25c3c16cbc7b97430",
        "87072c4d81d0fc5bed50e2dfac617c51bfc1f220",
        "d56bfdb800a51081383d933dfaad1285cf058fda",
        "8d68140402cd6581ae65a3af54ff787178f3859b",
        "443a774e42f5646bf0255f6cd668aa1935fc412e",
        "c1777c583a021288fda33d9eee22b4e3b92087d8",
        "07ed582c2a43d4859eb38871b65339a295b2906e",
        "ae63697e11de95a94598aa0486d3ae6f2a438d0f",
        "0ca56d65016aa159480206d7d91539c092566ea3",
        "201f97c8bf3a559f37e9dadb82e92c4180e3322d",
        "8c167c0ad59db1073c0c8826671e73ce0561adb4",
        "99bef9431032bc7806d4c34a495f7f4f67043444",
        "e38e6ea5bb4aa82e9acdb0849bf4e5f2efb78f28",
        "65abe69083ac9023fd3952fb2a8a5354e63b133e",
        "6b8ea6c308bc24a6bf8d955c24aeaccb7d4d9b06",
        "436c9314f8796d118b4bc95708526f11333a00a8",
        "87b8d9d95e1d8bef2174cda2163f5cf378d56c4c",
        "ad5f64c8bf437de6ee525a5f859457e5684d1665",
        "0688e6cac757aa271537b4049b72e879c54b6946",
        "9d592733e375494e047900a7c9ce3ecc37ad9ca0",
        "9edf1b03790570d6afdab2884522b9b1ae904604",
        "36dea16cd1dac349dd4dac5a650a1c86206fc3f7",
        "9bc930522bd3fdbefdcd8f6abfc500b920062d90",
        "c085aaa08781ab2dc1a5e378ee2d82f41ff820d0",
        "46f260e0cc2f9cb702fb391bf078bf22b58f81a0")
    }
  }

  protected def db: Managed[Throwable, VocabularyDb] =
    initDb(TestSheetIds.Simple).toManaged_

  private def initDb(sheetId: SheetId): Task[GsheetsVocabularyDb] =
    TestCfg.tokensDir.zip(TestCfg.cacheDir).flatMap { case (tokensDir, cacheDir) =>
      GsheetsVocabularyDb.make(sheetId, tokensDir, cacheDir)
    }.provide(ZEnvs.live)
}
