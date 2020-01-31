package com.github.mlangc.memento

import java.io.File

import zio.URIO
import zio.system
import zio.system.System


object TestCfg {
  def tokensDir: URIO[System, File] =
    system.property("user.home")
      .someOrFail(new RuntimeException("Cannot obtain HOME directory"))
      .map(new File(_, ".memento/test-tokens"))
      .orDie
}
