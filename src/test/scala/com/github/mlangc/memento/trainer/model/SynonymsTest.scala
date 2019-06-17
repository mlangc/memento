package com.github.mlangc.memento.trainer.model

import cats.data.NonEmptyList
import com.github.mlangc.memento.BaseTest
import com.github.mlangc.memento.db.model.Synonym
import com.github.mlangc.memento.db.model.Translation
import com.github.mlangc.memento.generators.DbGens
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SynonymsTest extends BaseTest with ScalaCheckPropertyChecks {
  private val dbGens = new DbGens()

  private def synonymsGen: Gen[Synonyms] = synonymsWithTranslationsGen.map(_._1)

  private def synonymsWithTranslationsGen: Gen[(Synonyms, List[Translation])] =
    for {
      translations <- Gen.listOf(dbGens.translation)
      synonyms1 <- dbGens.synonyms(translations.map(_.left))
      synonyms2 <- dbGens.synonyms(translations.map(_.right))
    } yield Synonyms.from(translations, synonyms1, synonyms2) -> translations

  "Extracting synonyms" - {
    "from trivial input" in {
      assert(Synonyms.from(
        List.empty[Translation],
        List.empty[Synonym],
        List.empty[Synonym]) === Synonyms.None)

      assert(Synonyms.from(
        List(Translation("test", "test")),
        List.empty[Synonym],
        List.empty[Synonym]) === Synonyms.None)
    }

    "synonyms without corresponding translations" in {
      assert(Synonyms.from(
        Translation("a", "b") :: Nil,
        Synonym("x", "y") :: Nil,
        Synonym("v", "w") :: Nil) === Synonyms.None)
    }

    "if a is a synonym of b, than b is a synonym of a" in {
      forAll(synonymsGen) { case Synonyms(left, right) =>
        List(left, right).foreach { syns =>
          val problems = syns.toSeq.flatMap { case (v, ws) =>
            ws.flatMap { w =>
              if (!syns(w).contains(v)) Some(v -> w)
              else None
            }
          }

          assert(problems.isEmpty)
        }
      }
    }

    "if a is a synonym of b and b is a synonym of c, then a is a synonym of c" in {
      forAll(synonymsGen) { case Synonyms(left, right) =>
        List(left, right).foreach { syns =>
          val problems = for {
            (a, bs) <- syns.toSeq
            b <- bs
            c <- syns(b) if !syns(c).contains(a)
          } yield (a, b, c)

          assert(problems.isEmpty)
        }
      }
    }

    "if a and b have the same translation, they are synonyms" in {
      forAll(synonymsWithTranslationsGen) { case (Synonyms(synsLeft, synsRight), translations) =>
        val synGroupsLeft = translations.groupBy(_.right).values
          .map(_.map(_.left))
          .collect { case w1 :: w2 :: ws => w1 -> NonEmptyList(w2, ws) }

        val synGroupsRight = translations.groupBy(_.left).values
          .map(_.map(_.right))
          .collect { case w1 :: w2 :: ws => w1 -> NonEmptyList(w2, ws) }

        val missingSynsLeft = synGroupsLeft.filter { case (w1, ws) =>
          ws.toList.toSet.subsetOf(synsLeft(w1))
        }

        val missingSynsRight = synGroupsLeft.filter { case (w1, ws) =>
          ws.toList.toSet.subsetOf(synsLeft(w1))
        }

        assert(missingSynsLeft.isEmpty)
        assert(missingSynsRight.isEmpty)
      }
    }
  }

}
