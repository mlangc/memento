package com.github.mlangc.memento.db.google.drive

import java.io.File

import com.github.mlangc.memento.BuildInfo
import com.github.mlangc.memento.db.google.GlobalJacksonFactory
import com.github.mlangc.memento.db.google.GlobalNetHttpTransport
import com.github.mlangc.memento.db.google.Gauthorizer
import com.google.api.client.auth.oauth2.Credential
import com.google.api.services.drive.Drive
import com.google.api.services.sheets.v4.SheetsScopes
import zio.RIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking


object GdriveService {
  def make(tokensDir: File, credential: Option[Credential] = None): RIO[Blocking, Drive] =
    GlobalJacksonFactory.get.zipPar(GlobalNetHttpTransport.get).flatMap { case (jacksonFactory, httpTransport) =>
      Gauthorizer.eventuallyAuthorize(credential, tokensDir, SheetsScopes.DRIVE).flatMap { credential =>
        effectBlocking {
          new Drive.Builder(httpTransport, jacksonFactory, credential)
            .setApplicationName(BuildInfo.name)
            .build()
        }
      }
    }
}
