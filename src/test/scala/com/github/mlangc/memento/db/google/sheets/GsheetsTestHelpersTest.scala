package com.github.mlangc.memento.db.google.sheets

import com.github.mlangc.memento.db.google.BaseGapiTest
import zio.Task
import zio.ZIO

class GsheetsTestHelpersTest extends BaseGapiTest {
  "Copy - delete" inIO {
    for {
      id0 <- ZIO.succeed(TestSheetIds.Simple)
      id1 <- GsheetsTestHelpers.copy(id0)
      _ <- GsheetsTestHelpers.delete(id1)
      v1 <- GsheetsTestHelpers.delete(id1).either
      _ <- Task(assert(v1.isLeft))
    } yield ()
  }
}
