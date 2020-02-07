package com.github.mlangc.memento.zenvs

import com.github.mlangc.memento.db.cache.CacheModule
import zio.blocking.Blocking
import zio.clock.Clock
import zio.system.System
import zio.console.Console

object ZEnvs {
  def live: CacheModule.Live with Blocking.Live with System.Live with Console.Live with Clock =
    new CacheModule.Live with Blocking.Live with System.Live with Console.Live with Clock.Live {}
}
