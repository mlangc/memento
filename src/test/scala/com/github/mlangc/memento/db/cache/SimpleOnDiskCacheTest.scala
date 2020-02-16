package com.github.mlangc.memento.db.cache

import com.github.mlangc.memento.test.util.TmpTestDir
import zio.Managed
import zio.ZIO
import zio.blocking.Blocking
import BasicCachables.intCachable
import BasicKeyables.intKeyable
import com.github.mlangc.memento.db.cache.MockLoader.Stats
import zio.Task

class SimpleOnDiskCacheTest extends SimpleCacheTest {
  def simpleCache: Managed[Throwable, SimpleCache] =
    for {
      dir <- TmpTestDir.make
      cache <- SimpleOnDiskCache.make(dir).provideLayer(Blocking.live)
    } yield cache

  "Verify eviction of not recently used entries" inIO {
    MockLoader.make[Int, Int, Nothing](a => ZIO.succeed(a)).flatMap { loader =>
      TmpTestDir.make.use { testDir =>
        val testCache = SimpleOnDiskCache.make(testDir).provideLayer(Blocking.live)

        testCache.use { cache =>
          for {
            v1 <- cache.load(1)(loader.load)
            v2 <- cache.load(2)(loader.load)
            _ <- cache.load(3)(loader.load)
            _ <- cache.load(4)(loader.load)
          } yield (v1, v2)
        }.flatMap { case (v1, v2) =>
          testCache.use { cache =>
            for {
              v3 <- cache.load(1)(loader.load)
              v4 <- cache.load(2)(loader.load)
              v5 <- cache.evictNotRecentlyUsed
              _ <- cache.load(1)(loader.load)
              v6 <- cache.load(3)(loader.load)
              s1 <- loader.stats(1)
              s2 <- loader.stats(3)
              _ <- Task {
                assert {
                  v1 == v3 && v2 == v4 && v5 == 2 && v6 == 3 &&
                    s1 == Stats(1, 1) && s2 == Stats(2, 2)
                }
              }
            } yield ()
          }
        }
      }
    }
  }
}
