package com.github.mlangc.memento.db.cache

import zio.RIO
import zio.Task

trait SimpleCache {
  def load[R, K : Keyable, A : Cachable](key: K)(f: K => RIO[R, A]): RIO[R, A]

  def evictNotRecentlyUsed: Task[Int]
  def evictAll: Task[Int]
}
