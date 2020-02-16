package com.github.mlangc.memento.zenvs

import com.github.mlangc.memento.db.cache.CacheModule
import zio.Has
import zio.ZLayer
import zio.blocking.Blocking
import zio.clock.Clock
import zio.scheduler.Scheduler
import zio.system.System

object ZLayers {
  type AppEnv = Has[CacheModule] with Blocking with Clock with System
  val live: ZLayer.NoDeps[Nothing, AppEnv] = CacheModule.live ++ Blocking.live ++ (Scheduler.live >>> Clock.live) ++ System.live
}

