package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.google.api.services.sheets.v4.Sheets
import zio.RIO
import zio.blocking
import zio.blocking.Blocking

private[sheets] object GsheetsService {
  def make(tokensDir: File): RIO[Blocking, Sheets] =
    GlobalJacksonFactory.get.zipPar(GlobalNetHttpTransport.get).flatMap { case (jacksonFactory, httpTransport) =>
      GsheetsAuthorizer.authorize(tokensDir).flatMap { credential =>
        blocking.effectBlocking {
          new Sheets.Builder(httpTransport, jacksonFactory, credential)
            .setApplicationName("memento")
            .build()
        }
      }
    }
}
