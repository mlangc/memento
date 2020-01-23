package com.github.mlangc.memento.util

import com.github.mlangc.memento.BuildInfo

object VersionInfo {
  def render: String = {
    val bi = BuildInfo
    val head = if (!bi.dirty) bi.head else s" uncommitted changes based on ${bi.head} build at ${bi.builtAtString}"
    s"${bi.name}-${bi.version} (built from $head)"
  }
}
