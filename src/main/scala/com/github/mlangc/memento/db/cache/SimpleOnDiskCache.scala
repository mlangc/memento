package com.github.mlangc.memento.db.cache
import java.io.File

import com.github.ghik.silencer.silent
import zio.Task
import zio.ZIO
import zio.ZManaged
import zio.blocking.Blocking


class SimpleOnDiskCache private (@silent blocking: Blocking) extends SimpleCache {
  def load[K: Keyable, A: Cachable](key: K)(f: K => Task[A]): Task[A] = f(key)
  def evictNotRecentlyUsed: Task[Int] = Task(0)
}


object SimpleOnDiskCache {
  def make(@silent path: File): ZManaged[Blocking, Throwable, SimpleCache] =
    ZIO.access[Blocking](b => new SimpleOnDiskCache(b)).toManaged_
}
