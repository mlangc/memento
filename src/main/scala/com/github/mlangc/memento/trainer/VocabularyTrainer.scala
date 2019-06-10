package com.github.mlangc.memento.trainer

import com.github.mlangc.memento.db.VocabularyDb
import com.github.mlangc.memento.trainer.examiner.Examiner
import scalaz.zio.Task

trait VocabularyTrainer {
  def train(db: VocabularyDb, examiner: Examiner): Task[Unit]
}
