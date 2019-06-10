package com.github.mlangc.memento.trainer.examiner

import com.github.mlangc.memento.BaseTest
import com.github.mlangc.memento.db.model.Direction
import com.github.mlangc.memento.db.model.Score
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.trainer.model.Answer
import com.github.mlangc.memento.trainer.model.Hint
import com.github.mlangc.memento.trainer.model.Question
import com.github.mlangc.memento.trainer.model.Synonyms

import eu.timepit.refined.auto._

class ExaminerTest extends BaseTest {
  "Scoring" - {
    "with a blank answer" in {
      assert {
        Examiner.score(
          Question(Translation("test", "test"), Direction.RightToLeft, None),
          Answer.Blank, 0, Synonyms.None) === Score.Zero
      }
    }

    "with the right answer" in {
      assert {
        Examiner.score(
          Question(Translation("a", "b"), Direction.LeftToRight, None),
          Answer.Text("b"), 0, Synonyms.None) === Score.Perfect
      }

      assert {
        Examiner.score(
          Question(Translation("a", "b"), Direction.RightToLeft, None),
          Answer.Text("a"), 0, Synonyms.None) === Score.Perfect
      }
    }

    "with the wrong answer" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, None),
          Answer.Text("asdf"), 0, Synonyms.None) === Score.Zero
      }
    }

    "with almost an almost right answer" ignore {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.LeftToRight, None),
          Answer.Text("scalywag"), 0, Synonyms.None) === Score.SoSo
      }
    }

    "with a single hint" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__", 0.1))),
          Answer.Text("Lausbub"), 0, Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__", 0.1))),
          Answer.Text("Lauub"), 0, Synonyms.None) === Score.Zero
      }
    }

    "with two hints" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__b", 0.1))),
          Answer.Text("Lausbub"), 1, Synonyms.None) === Score.Poor
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__b", 0.1))),
          Answer.Text("Laub"), 1, Synonyms.None) === Score.Zero
      }
    }
  }
}
