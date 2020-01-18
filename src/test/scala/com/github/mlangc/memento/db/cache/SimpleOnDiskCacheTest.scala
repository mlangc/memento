package com.github.mlangc.memento.db.cache
import com.github.mlangc.memento.test.util.TmpTestDir
import zio.Managed
import zio.blocking.Blocking

class SimpleOnDiskCacheTest extends SimpleCacheTest {
  def simpleCache: Managed[Throwable, SimpleCache] =
    for {
      dir <- TmpTestDir.make
      cache <- SimpleOnDiskCache.make(dir).provide(Blocking.Live)
    } yield cache
}
