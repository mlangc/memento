package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import zio.Managed
import zio.Task
import eu.timepit.refined.auto._
import com.github.ghik.silencer.silent
import zio.blocking.Blocking

@silent("`Any`")
class GsheetsVocabularyDbTest extends GenericVocabularyDbTest {
  private def testSheetIdVar = "TEST_SHEET_ID"
  private def credentialsPathVar = "GOOGLE_CREDENTIALS_PATH"

  private lazy val sheetId = ciris.env[String](testSheetIdVar).value.right.getOrElse("")
  private lazy val credentialsPath = ciris.env[String](credentialsPathVar).value.right.getOrElse("")

  protected def db: Managed[Throwable, VocabularyDb] = {
    Managed.fromEffect {
      Task {
        assume(sheetId.nonEmpty, s"Please set $testSheetIdVar")
        assume(credentialsPath.nonEmpty, s"Please set $credentialsPathVar")
      } *> GsheetsVocabularyDb.make(sheetId, new File(credentialsPath)).provide(Blocking.Live)
    }
  }

  "Verify that data is loaded correctly" inIO {
    db.use { db =>
      db.load.flatMap { data =>
        Task {
          assert(data.translations.contains(Translation("ich", "je")))
          assert(data.translations.contains(Translation("sie", "elle")))
          assert(data.checks.filter(_.translation == Translation("test", "test")).nonEmpty)
          assert(data.synonyms1.contains(Synonym("dumm", "blöd")))
          assert(data.synonyms2.contains(Synonym("forêt", "bois")))
          assert(data.language1.value === "Deutsch")
          assert(data.language2.value === "Französisch")
        }
      }
    }
  }
}
