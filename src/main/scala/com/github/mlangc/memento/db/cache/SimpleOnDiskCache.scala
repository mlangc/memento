package com.github.mlangc.memento.db.cache
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption

import eu.timepit.refined.refineV
import org.apache.commons.io.FileUtils
import zio.Ref
import zio.Task
import zio.ZIO
import zio.ZManaged
import zio.blocking.Blocking

class SimpleOnDiskCache private (cacheDir: File, blocking: Blocking.Service[Any], usedRef: Ref[Set[CacheKey]]) extends SimpleCache {
  import blocking._

  def load[K: Keyable, A: Cachable](k: K)(f: K => Task[A]): Task[A] = {
    val keyable = implicitly[Keyable[K]]
    val cacheable = implicitly[Cachable[A]]

    val key = keyable.toKey(k)
    val cacheFile = cacheFileFor(key)

    def loadAndCache: Task[A] =
      for {
        a <- f(k)
        bytes = cacheable.toBytes(a)
        _ <- effectBlocking {
          val tmpFile = Files.createTempFile(cacheDir.toPath, ".tmp-", "").toFile
          try {
            FileUtils.writeByteArrayToFile(tmpFile, bytes)
            Files.move(tmpFile.toPath, cacheFile.toPath, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
          } finally {
            FileUtils.deleteQuietly(tmpFile)
            ()
          }
        }
      } yield a

    def cachedLoad: Task[A] =
      for {
        bytes <- blocking.effectBlocking(FileUtils.readFileToByteArray(cacheFile))
        a <- ZIO.fromEither(cacheable.fromBytes(bytes))
      } yield a

    Task(cacheFile.canRead && cacheFile.isFile).flatMap {
      case false => loadAndCache
      case true => cachedLoad
    } <* usedRef.update(_ + key)
  }

  def evictNotRecentlyUsed: Task[Int] =
    allKeys.zip(usedRef.get).flatMap { case (keys, used) =>
      val unused = keys -- used
      ZIO.foldLeft(unused)(0) { (s, key) =>
        val file = cacheFileFor(key)
        effectBlocking(file.delete()).map {
          case true => s + 1
          case false => s
        }
      }
    }

  def evictAll: Task[Int] =
    allKeys.flatMap(evict)

  private def cacheFileFor(key: CacheKey): File =
    new File(cacheDir, key.toString())

  private def evict(keys: Set[CacheKey]): Task[Int] =
    effectBlocking {
      keys.foldLeft(0) { (n, key) =>
        if (FileUtils.deleteQuietly(cacheFileFor(key))) n + 1 else n
      }
    }

  private def allKeys: Task[Set[CacheKey]] =
    effectBlocking(cacheDir.listFiles(f => f.isFile && f.canRead && !f.getName.startsWith(".")))
      .map { files =>
        files.foldLeft(Set.empty[CacheKey]) { (set, file) =>
          set ++ refineV[CacheKeyRefinement](file.getName).toOption.toSet
        }
      }
}


object SimpleOnDiskCache {
  def make(cacheDir: File): ZManaged[Blocking, Throwable, SimpleCache] = {
    for {
      blocking <- ZIO.access[Blocking](identity)
      usedRef <- Ref.make(Set.empty[CacheKey])
      _ <- ZIO.whenM(Task(!cacheDir.exists()))(Task(FileUtils.forceMkdir(cacheDir)))
    } yield new SimpleOnDiskCache(cacheDir, blocking.blocking, usedRef)
  }.toManaged_
}
