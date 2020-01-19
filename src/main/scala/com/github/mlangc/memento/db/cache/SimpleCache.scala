package com.github.mlangc.memento.db.cache

import zio.Task

trait SimpleCache {
  def load[K : Keyable, A : Cachable](key: K)(f: K => Task[A]): Task[A]

  def evictNotRecentlyUsed: Task[Int]
  def evictAll: Task[Int]
}
