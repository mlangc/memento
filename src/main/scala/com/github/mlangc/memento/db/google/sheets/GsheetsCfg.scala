package com.github.mlangc.memento.db.google.sheets

import ciris.ConfigEntry
import ciris.api.Id

object GsheetsCfg {
  def sheetId: ConfigEntry[Id, String, String, String] = ciris.env[String]("SHEET_ID")
  def credentialsPath: ConfigEntry[Id, String, String, String] = ciris.env[String]("GOOGLE_CREDENTIALS_PATH")
}
