package com.github.mlangc.memento.db.google

import com.google.api.client.json.jackson2.JacksonFactory
import zio.UIO

private object GlobalJacksonFactory {
  private lazy val jacksonFactory = JacksonFactory.getDefaultInstance
  val get: UIO[JacksonFactory] = UIO(jacksonFactory)
}
