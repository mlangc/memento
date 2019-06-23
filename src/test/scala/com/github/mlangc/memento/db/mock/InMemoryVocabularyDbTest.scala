package com.github.mlangc.memento.db.mock

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.db.GenericVocabularyDbTest
import com.github.mlangc.memento.db.model.TestVocabularyData
import zio.Managed

class InMemoryVocabularyDbTest extends GenericVocabularyDbTest {
  protected def db: Managed[Throwable, VocabularyDb] = Managed.fromEffect {
    InMemoryVocabularyDb.make(TestVocabularyData.gerFrSimple)
  }
}
