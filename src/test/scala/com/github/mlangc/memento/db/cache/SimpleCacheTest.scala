package com.github.mlangc.memento.db.cache

import com.github.mlangc.memento.BaseZioTest
import zio.Managed
import zio.Task
import zio.ZIO
import BasicCachables.shortCachable
import BasicKeyables.intKeyable
import com.github.mlangc.memento.db.cache.MockLoader.Stats

abstract class SimpleCacheTest extends BaseZioTest {
  def simpleCache: Managed[Throwable, SimpleCache]

  "Make sure that our cache behaves properly" inIO {
    simpleCache.use { cache =>
      for {
        v1 <- cache.evictNotRecentlyUsed

        loader <- MockLoader.make[Int, Short, IllegalArgumentException] { i =>
          if (i <= Short.MaxValue && i >= Short.MinValue) ZIO.succeed(i.toShort)
          else ZIO.fail(new IllegalArgumentException(s"Could not convert $i to short"))
        }

        v2 <- cache.load(1)(loader.load)
        v3 <- loader.stats(1)

        _ <- Task {
          assert {
            v1 == 0 && v2 == 1.toShort && v3 == Stats(1, 1)
          }
        }
      } yield ()
    }
  }
}
