package com.github.mlangc.memento.trainer.repetition.random

import com.github.mlangc.memento.trainer.repetition.GenericRepetitionSchemeTest
import com.github.mlangc.memento.trainer.repetition.RepetitionScheme
import scalaz.zio.Task

class RandomRepetitionSchemeTest extends GenericRepetitionSchemeTest {
  protected def scheme: Task[RepetitionScheme] = Task.succeed(RandomRepetitionScheme)
}
