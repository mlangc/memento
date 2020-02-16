package com.github.mlangc.memento.db.cache

import java.io.File

import zio.Has
import zio.ZLayer
import zio.ZManaged
import zio.blocking.Blocking


trait CacheModule {
  def makeSimpleCache(cacheDir: File): ZManaged[Blocking, Throwable, SimpleCache]
}

object CacheModule {
  val live: ZLayer.NoDeps[Nothing, Has[CacheModule]] = ZLayer.succeed {
    (cacheDir: File) => SimpleOnDiskCache.make(cacheDir)
  }
}
