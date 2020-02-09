package com.github.mlangc.memento.zenvs

import com.github.mlangc.memento.db.cache.CacheModule
import zio.blocking.Blocking
import zio.clock.Clock
import zio.system.System
import zio.console.Console

object ZEnvs {
  type AppEnv = CacheModule.Live with Blocking.Live with System.Live with Console.Live with Clock

  def live: AppEnv =
    new CacheModule.Live with Blocking.Live with System.Live with Console.Live with Clock.Live {}
}
