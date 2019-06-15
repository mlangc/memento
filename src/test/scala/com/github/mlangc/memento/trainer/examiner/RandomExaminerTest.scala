package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.trainer.repetition.random.RandomRepetitionScheme

class RandomExaminerTest extends GenericExaminerTest {
  protected val examiner = new DefaultExaminer(RandomRepetitionScheme)
}
