package com.github.mlangc.memento.db.google.sheets

import java.io.File
import java.io.InputStreamReader
import java.util.Collections

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.services.sheets.v4.SheetsScopes
import zio.RIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

object GsheetsAuthorizer {
  def authorize(tokensDir: File): RIO[Blocking, Credential] =
    GlobalJacksonFactory.get.zipPar(GlobalNetHttpTransport.get).flatMap { case (jacksonFactory, httpTransport) =>
      effectBlocking {
        val secretsIn = getClass.getResourceAsStream("/oauth-client-credentials.json")
        try {
          val clientSecrets = GoogleClientSecrets.load(jacksonFactory, new InputStreamReader(secretsIn))
          val scopes = Collections.singletonList(SheetsScopes.SPREADSHEETS)

          val flow = new GoogleAuthorizationCodeFlow.Builder(httpTransport, jacksonFactory, clientSecrets, scopes)
            .setDataStoreFactory(new FileDataStoreFactory(tokensDir))
            .setAccessType("offline")
            .build()

          val receiver = new LocalServerReceiver.Builder().setPort(-1).build()
          new AuthorizationCodeInstalledApp(flow, receiver).authorize("user")
        } finally {
          secretsIn.close()
        }
      }
    }
}
