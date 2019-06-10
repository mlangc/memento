package com.github.mlangc.memento.trainer.examiner.random

import com.github.mlangc.memento.trainer.examiner.Examiner
import com.github.mlangc.memento.trainer.examiner.GenericExaminerTest
import scalaz.zio.Managed
import scalaz.zio.UIO

class RandomExaminerTest extends GenericExaminerTest {
  protected def mkExaminer: Managed[Nothing, Examiner] = Managed.fromEffect(UIO.succeed(new RandomExaminer))
}
