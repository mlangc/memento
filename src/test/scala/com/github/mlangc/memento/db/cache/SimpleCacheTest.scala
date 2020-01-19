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
        v1 <- cache.evictAll

        loader <- MockLoader.make[Int, Short, IllegalArgumentException] { i =>
          if (i <= Short.MaxValue && i >= Short.MinValue) ZIO.succeed(i.toShort)
          else ZIO.fail(new IllegalArgumentException(s"Could not convert $i to short"))
        }
        cachedLoad = (i: Int) => cache.load(i)(loader.load)

        v2 <- cachedLoad(1)
        s1 <- loader.stats(1)
        v3 <- cachedLoad(1)
        s2 <- loader.stats(1)
        v4 <- cachedLoad(2)
        v5 <- cachedLoad(2)
        v6 <- cachedLoad(1)
        s3 <- loader.stats(2)
        v7 <- cache.evictAll
        v8 <- cachedLoad(2)
        s4 <- loader.stats(2)
        v9 <- cache.evictNotRecentlyUsed
        v10 <- cachedLoad(2)
        s5 <- loader.stats(2)

        _ <- Task {
          assert {
            v1 == 0 && v2 == 1.toShort && s1 == Stats(1, 1) &&
            v3 == v2 && s1 == s2 &&
            v4 == 2.toShort && v4 == v5 && v6 == v2 && s3 == s2 &&
            v7 == 2 && v8 == 2.toShort && s4 == Stats(2, 2) &&
            v9 == 0 && v10 == v8 && s5 == s4
          }
        }
      } yield ()
    }
  }
}
