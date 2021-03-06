package com.github.mlangc.memento.db

import java.time.Instant

import com.github.mlangc.memento.db.google.BaseGapiTest
import com.github.mlangc.memento.db.model.Check
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Translation
import eu.timepit.refined.auto._
import zio.Managed
import zio.Task

abstract class GenericVocabularyDbTest extends BaseGapiTest {
  protected def db: Managed[Throwable, VocabularyDb]

  "Make sure that our DB behaves sane" inIO {
    db.use { db =>
      for {
        now <- Task(Instant.now())
        data1 <- db.load
        check = Check(Translation("test1", "test2"), Direction.LeftToRight, Score.Perfect, now)
        _ <- db.addCheck(check)
        data2 <- db.load
        _ <- Task {
          assert(data2 === data1.copy(checks = data1.checks :+ check))
        }
      } yield ()
    }
  }
}
