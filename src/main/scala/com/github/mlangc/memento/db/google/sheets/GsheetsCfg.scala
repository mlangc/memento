package com.github.mlangc.memento.db.google.sheets

import ciris.ConfigErrors
import ciris.api.Id
import ciris.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

case class GsheetsCfg private(sheetId: String Refined NonEmpty,
                              credentialsPath: String Refined NonEmpty)

object GsheetsCfg {
  def load: Either[ConfigErrors, GsheetsCfg] =
    ciris.loadConfig(
      ciris.env[String Refined NonEmpty]("SHEET_ID"),
      ciris.env[String Refined NonEmpty]("GOOGLE_CREDENTIALS_PATH")
    )(GsheetsCfg(_, _)).result
}
