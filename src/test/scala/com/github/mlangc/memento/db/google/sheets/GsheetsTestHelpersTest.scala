package com.github.mlangc.memento.db.google.sheets

import com.github.mlangc.memento.BaseZioTest
import com.github.mlangc.memento.TestCfg
import zio.RIO
import zio.Task
import zio.system.System

class GsheetsTestHelpersTest extends BaseZioTest {
  private def testSheetIdEnvVar = "TEST_SHEET_ID"

  private def testSheetsId: RIO[System, SheetId] =
    TestCfg.sheetId(testSheetIdEnvVar)
      .catchAll(errors => Task(cancel(s"Unable to load sheet id from $testSheetIdEnvVar: $errors")))

  "Copy - delete" inIO {
    for {
      id0 <- testSheetsId
      id1 <- GsheetsTestHelpers.copy(id0)
      _ <- GsheetsTestHelpers.delete(id1)
      v1 <- GsheetsTestHelpers.delete(id1).either
      _ <- Task(assert(v1.isLeft))
    } yield ()
  }
}
