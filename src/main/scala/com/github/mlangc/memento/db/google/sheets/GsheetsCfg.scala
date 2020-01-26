package com.github.mlangc.memento.db.google.sheets

import java.io.File

import scala.util.control.NonFatal

import cats.syntax.either._
import ciris.ConfigError
import ciris.ConfigErrors
import ciris.api.Id
import ciris.refined._
import eu.timepit.refined.refineV
import zio.IO
import zio.ZIO


case class GsheetsCfg private(sheetId: SheetId,
                              tokensPath: TokensPath)

object GsheetsCfg {
  def load: IO[ConfigErrors, GsheetsCfg] =
    load("SHEET_ID", "GOOGLE_TOKENS_PATH")

  def load(sheetEnvVar: String, tokensPathEnvVar: String): IO[ConfigErrors, GsheetsCfg] = ZIO.fromEither {
    ciris.loadConfig(
      ciris.env[SheetId](sheetEnvVar),
      ciris.env[TokensPath](tokensPathEnvVar).orElse(ciris.prop[String]("user.home").flatMapValue(defaultTokensPath))
    )(GsheetsCfg(_, _)).result
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
