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
          Answer.Blank, Synonyms.None) === Score.Zero
      }
    }

    "with the right answer" in {
      assert {
        Examiner.score(
          Question(Translation("a", "b"), Direction.LeftToRight, None),
          Answer.Text("b"), Synonyms.None) === Score.Perfect
      }

      assert {
        Examiner.score(
          Question(Translation("a", "b"), Direction.RightToLeft, None),
          Answer.Text("a"), Synonyms.None) === Score.Perfect
      }
    }

    "with the wrong answer" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, None),
          Answer.Text("asdf"), Synonyms.None) === Score.Zero
      }
    }

    "with almost almost right answers" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.LeftToRight, None),
          Answer.Text("scalywag"), Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("verbreitet", "répandu"), Direction.LeftToRight, None),
          Answer.Text("repandu"), Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, None),
          Answer.Text("lausbub"), Synonyms.None) === Score.Good
      }

      assert {
        Examiner.score(
          Question(Translation("Gundwasser", "une nappe phréatique"), Direction.LeftToRight, None),
          Answer.Text("une nappe phreatique"), Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("Gundwasser", "une nappe phréatique"), Direction.LeftToRight, None),
          Answer.Text("une nappe phreantique"), Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("Gundwasser", "une nappe phréatique"), Direction.LeftToRight, None),
          Answer.Text("un nappe phreantique"), Synonyms.None) === Score.Poor
      }

      assert {
        Examiner.score(
          Question(Translation("Gundwasser", "une nappe phréatique"), Direction.LeftToRight, None),
          Answer.Text("un nape phreantique"), Synonyms.None) === Score.Poor
      }

      assert {
        Examiner.score(
          Question(Translation("Gundwasser", "une nappe phréatique"), Direction.LeftToRight, None),
          Answer.Text("un nap preanice"), Synonyms.None) === Score.Zero
      }
    }

    "with small hints" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__", 0.15))),
          Answer.Text("Lausbub"), Synonyms.None) === Score.Good
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__", 0.15))),
          Answer.Text("Lauub"), Synonyms.None) === Score.Zero
      }
    }

    "with bigger hints" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__b", 0.30))),
          Answer.Text("Lausbub"), Synonyms.None) === Score.SoSo
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L__b", 0.30))),
          Answer.Text("Laub"), Synonyms.None) === Score.Zero
      }
    }

    "with strong hints" in {
      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("L_ub", 0.45))),
          Answer.Text("Lausbub"), Synonyms.None) === Score.Poor
      }

      assert {
        Examiner.score(
          Question(Translation("Lausbub", "scallywag"), Direction.RightToLeft, Some(Hint("La_ub", 0.55))),
          Answer.Text("Lausbub"), Synonyms.None) === Score.Zero
      }
    }
  }
}
