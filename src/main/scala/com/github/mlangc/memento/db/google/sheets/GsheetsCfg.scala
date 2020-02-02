package com.github.mlangc.memento.db.google.sheets

import java.io.File

import scala.util.control.NonFatal
import cats.syntax.either._
import ciris.ConfigEntry
import ciris.ConfigError
import ciris.ConfigErrors
import ciris.api.Id
import ciris.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.api.Validate
import eu.timepit.refined.refineV
import zio.IO
import zio.UIO
import zio.ZIO


case class GsheetsCfg private(sheetId: SheetId,
                              tokensPath: TokensPath,
                              cachePath: CachePath)

object GsheetsCfg {
  def load: IO[ConfigErrors, GsheetsCfg] =
    load("SHEET_ID", "GOOGLE_TOKENS_PATH", "CACHE_PATH")

  def load(sheetEnvVar: String, tokensPathEnvVar: String, cachePathEnvVar: String): IO[ConfigErrors, GsheetsCfg] = ZIO.fromEither {
    ciris.loadConfig(
      ciris.env[SheetId](sheetEnvVar),
      ciris.env[TokensPath](tokensPathEnvVar).orElse(defaultTokensPath),
      ciris.env[CachePath](cachePathEnvVar).orElse(defaultCachePath)
    )(GsheetsCfg(_, _, _)).result
  }

  def loadDefaultTokensPath: UIO[TokensPath] =
    ZIO.fromEither(defaultTokensPath.value)
      .orDieWith(err => new RuntimeException(s"Cannot load default tokensPath: ${err.message}"))

  private def defaultTokensPath: ConfigEntry[Id, String, String, TokensPath] =
    defaultPathConfig[TokensPathRefinement]("tokens")

  private def defaultCachePath: ConfigEntry[Id, String, String, CachePath] =
    defaultPathConfig[CachePathRefinement]("cache")

  private def defaultPathConfig[P](subPath: String)
                                  (implicit validate: Validate[String, P]): ConfigEntry[Id, String, String, String Refined P] =
    ciris.prop[String]("user.home").flatMapValue(defaultPath[P](_, subPath))

  private def defaultPath[P](userHome: String, subPath: String)(implicit validate: Validate[String, P]): Either[ConfigError, String Refined P] =
    try {
      refineV[P](new File(new File(userHome), s".memento/$subPath").getPath)
        .leftMap(msg => ConfigError(msg))
    } catch {
      case NonFatal(e) =>
        ConfigError(e.toString).asLeft
    }
}
