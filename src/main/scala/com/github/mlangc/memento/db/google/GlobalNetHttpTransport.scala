package com.github.mlangc.memento.db.google

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.javanet.NetHttpTransport
import zio.RIO
import zio.blocking.Blocking
import zio.blocking.effectBlocking

private object GlobalNetHttpTransport {
  private lazy val transport = GoogleNetHttpTransport.newTrustedTransport()
  val get: RIO[Blocking, NetHttpTransport] = effectBlocking(transport)
}
