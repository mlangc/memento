package com.github.mlangc.memento.db.google

import java.io.File
import java.io.InputStreamReader
import java.util

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.util.store.FileDataStoreFactory
import zio.RIO
import zio.ZIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

import scala.jdk.CollectionConverters._


private object Gauthorizer {
  def authorize(tokensDir: File, scope: String, scopes: String*): RIO[Blocking, Credential] =
    GlobalJacksonFactory.get.zipPar(GlobalNetHttpTransport.get).flatMap { case (jacksonFactory, httpTransport) =>
      effectBlocking {
        val secretsIn = {
          val tmp = getClass.getResourceAsStream("/oauth-client-credentials-test.json")
          if (tmp ne null) tmp else getClass.getResourceAsStream("/oauth-client-credentials.json")
        }

        if (secretsIn == null)
          throw new IllegalStateException("Cannot load OAuth 2.0 client secret")

        try {
          val clientSecrets = GoogleClientSecrets.load(jacksonFactory, new InputStreamReader(secretsIn))
          val scopesList: util.List[String] = (Array(scope) ++ scopes.toArray).toSeq.asJava

          val flow = new GoogleAuthorizationCodeFlow.Builder(httpTransport, jacksonFactory, clientSecrets, scopesList)
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

  def eventuallyAuthorize(credential: Option[Credential], tokensDir: File, scope: String, scopes: String*): RIO[Blocking, Credential] =
    ZIO.fromOption(credential).orElse(authorize(tokensDir, scope, scopes: _*))
}
