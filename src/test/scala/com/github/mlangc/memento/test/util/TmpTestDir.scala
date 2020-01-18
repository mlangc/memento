package com.github.mlangc.memento.test.util

import java.io.File
import java.nio.file.Files

import org.apache.commons.io.FileUtils
import zio.Managed

object TmpTestDir {
  def make: Managed[Throwable, File] =
    Managed.makeEffect(Files.createTempDirectory("test-").toFile)(FileUtils.deleteQuietly)
}
