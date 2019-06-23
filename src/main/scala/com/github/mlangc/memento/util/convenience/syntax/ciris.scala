package com.github.mlangc.memento.util.convenience.syntax

import _root_.ciris.ConfigEntry
import _root_.ciris.api.Id
import zio.UIO

object ciris {
  implicit class ConfigEntryIdOps[K, S, V](val configEntry: ConfigEntry[Id, K, S, V]) extends AnyVal {
    def orDie: UIO[V] = configEntry.value match {
      case Right(r) => UIO.succeed(r)
      case Left(_) =>
        UIO.dieMessage(s"Cannot load config value ${configEntry.key} from ${configEntry.keyType}")
    }
  }
}
