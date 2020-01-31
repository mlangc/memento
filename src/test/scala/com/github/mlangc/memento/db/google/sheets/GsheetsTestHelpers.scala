package com.github.mlangc.memento.db.google.sheets

import zio.Task

private[sheets] class GsheetsTestHelpers {

}

private[sheets] object GsheetsTestHelpers {
  def copy(id: SheetId): Task[SheetId] = ???
  def delete(id: SheetId): Task[Unit] = ???
}
