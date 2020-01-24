package com.github.mlangc.memento.db.google.sheets

import java.io.File

import cats.syntax.either._
import ciris.ConfigError
import ciris.ConfigErrors
import ciris.api.Id
import ciris.refined._
import eu.timepit.refined.refineV
import zio.IO
import zio.ZIO

import scala.util.control.NonFatal


case class GsheetsCfg private(sheetId: SheetId,
                              credentialsPath: CredentialsPath,
                              tokensPath: TokensPath)

object GsheetsCfg {
  def load: IO[ConfigErrors, GsheetsCfg] = ZIO.fromEither {
    ciris.loadConfig(
      ciris.env[SheetId]("SHEET_ID"),
      ciris.env[CredentialsPath]("GOOGLE_CREDENTIALS_PATH"),
      ciris.env[TokensPath]("GOOGLE_TOKENS_PATH").orElse(ciris.prop[String]("user.home").flatMapValue(defaultTokensPath))
    )(GsheetsCfg(_, _, _)).result
  }

  private def defaultTokensPath(userHome: String): Either[ConfigError, TokensPath] =
    try {
      refineV[TokensPathRefinement](new File(new File(userHome), ".memento/tokens").getAbsolutePath)
        .leftMap(msg => ConfigError(msg))
    } catch {
      case NonFatal(e) =>
        ConfigError(e.toString).asLeft
    }
}
