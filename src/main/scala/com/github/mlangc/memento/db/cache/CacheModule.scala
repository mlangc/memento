package com.github.mlangc.memento.db.cache

import java.io.File

import zio.ZManaged
import zio.blocking.Blocking


trait CacheModule {
  def makeSimpleCache(cacheDir: File): ZManaged[Blocking, Throwable, SimpleCache]
}

object CacheModule {
  trait Live extends CacheModule {
    def makeSimpleCache(cacheDir: File): ZManaged[Blocking, Throwable, SimpleCache] =
      SimpleOnDiskCache.make(cacheDir)
  }
}
