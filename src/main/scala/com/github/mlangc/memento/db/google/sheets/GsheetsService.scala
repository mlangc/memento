package com.github.mlangc.memento.db.google.sheets

import java.io.File

import com.github.mlangc.memento.BuildInfo
import com.github.mlangc.memento.db.google.GlobalJacksonFactory
import com.github.mlangc.memento.db.google.GlobalNetHttpTransport
import com.github.mlangc.memento.db.google.Gauthorizer
import com.google.api.client.auth.oauth2.Credential
import com.google.api.services.sheets.v4.Sheets
import com.google.api.services.sheets.v4.SheetsScopes
import zio.RIO
import zio.blocking
import zio.blocking.Blocking

private[sheets] object GsheetsService {
  def make(tokensDir: File, credential: Option[Credential] = None): RIO[Blocking, Sheets] =
    GlobalJacksonFactory.get.zipPar(GlobalNetHttpTransport.get).flatMap { case (jacksonFactory, httpTransport) =>
      Gauthorizer.eventuallyAuthorize(credential, tokensDir, SheetsScopes.SPREADSHEETS).flatMap { credential =>
        blocking.effectBlocking {
          new Sheets.Builder(httpTransport, jacksonFactory, credential)
            .setApplicationName(BuildInfo.name)
            .build()
        }
      }
    }
}
