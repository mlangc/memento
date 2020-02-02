package com.github.mlangc.memento.db.google.sheets

import com.github.mlangc.memento.db.cache.CacheKeyRefinement
import com.github.mlangc.memento.db.cache.Keyable
import eu.timepit.refined.refineV

private[sheets] case class HashedRangeCacheKey(hash: Sha1, index: Int) {
  def range(sheet: String, leftCol: String, rightCol: String, step: Int, offset: Int): String = {
    val upperRow = index * step + offset + 1
    val lowerRow = upperRow + step - 1
    s"$sheet!$leftCol$upperRow:$rightCol$lowerRow"
  }
}

private[sheets] object HashedRangeCacheKey {
  implicit val hashedRangeCacheKeyKeyable: Keyable[HashedRangeCacheKey] =
    k => refineV[CacheKeyRefinement](k.hash.value).getOrElse(throw new AssertionError(s"Error extracting cache key: $k"))
}
