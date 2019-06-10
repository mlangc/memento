package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import scalaz.zio.Managed
import scalaz.zio.Task

class GsheetsVocabularyDbTest extends GenericVocabularyDbTest {
  private val sheetId = ciris.env[String]("TEST_SHEET_ID").value.right.getOrElse("")
  private val credentialsPath = ciris.env[String]("GOOGLE_CREDENTIALS_PATH").value.right.getOrElse("")

  protected def db: Managed[Throwable, VocabularyDb] = Managed.fromEffect {
    assume(sheetId.nonEmpty)
    assume(credentialsPath.nonEmpty)

    GsheetsVocabularyDb.make(sheetId, new File(credentialsPath))
  }

  "Verify that data is loaded correctly" in {
    unsafeRun {
      db.use { db =>
        db.load.flatMap { data => Task {
          assert(data.translations.contains(Translation("ich", "je")))
          assert(data.translations.contains(Translation("sie", "elle")))
          assert(data.checks.filter(_.translation == Translation("test", "test")).nonEmpty)
          assert(data.synonyms1.contains(Synonym("dumm", "blöd")))
          assert(data.synonyms2.contains(Synonym("forêt", "bois")))
          assert(data.language1 === "Deutsch")
          assert(data.language2 === "Französisch")
        }}
      }
    }
  }
}
