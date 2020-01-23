package com.github.mlangc.memento.db.google.sheets

import com.google.api.client.json.jackson2.JacksonFactory
import zio.UIO

private[sheets] object GlobalJacksonFactory {
  private lazy val jacksonFactory = JacksonFactory.getDefaultInstance
  val get: UIO[JacksonFactory] = UIO(jacksonFactory)
}
