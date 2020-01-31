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
import zio.blocking.Blocking
import zio.system.System


@silent("inferred to be `Any`")
class GsheetsVocabularyDbTest extends GenericVocabularyDbTest {
  private def testSheetIdVar = "TEST_SHEET_ID"

  protected def db: Managed[Throwable, VocabularyDb] =
    TestCfg.tokensDir.zip(TestCfg.sheetId(testSheetIdVar))
      .catchAll(e => Task(cancel(s"Error loading configuration: $e")))
      .flatMap { case (tokensDir, sheetId) =>
        GsheetsVocabularyDb.make(sheetId, tokensDir)
      }
      .provide(new Blocking.Live with System.Live {})
      .toManaged_

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
}
