package com.github.mlangc.memento

import java.io.File

import zio.URIO
import zio.system
import zio.system.System


object TestCfg {
  def tokensDir: URIO[System, File] = testDir("tokens")
  def cacheDir: URIO[System, File] = testDir("cache")

  private def testDir(name: String): URIO[System, File] =
    system.property("user.home")
      .someOrFail(new RuntimeException("Cannot obtain HOME directory"))
      .map(new File(_, s".memento/test/$name"))
      .orDie
}
