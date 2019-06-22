package com.github.mlangc.memento.trainer.model

import cats.data.NonEmptyList
import com.github.mlangc.memento.BaseTest
import com.github.mlangc.memento.db.model.{Synonym, Translation, Vocabulary}
import com.github.mlangc.memento.generators.DbGens
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import eu.timepit.refined.auto._

class SynonymsTest extends BaseTest with ScalaCheckPropertyChecks {
  private val dbGens = new DbGens()

  private def synonymsGen: Gen[Synonyms] = synonymsWithTranslationsGen.map(_._1)

  private def synonymsWithTranslationsGen: Gen[(Synonyms, List[Translation])] =
    synonymsWithInputGen.map { case (synonyms, _, _, translation) => synonyms -> translation }

  private def synonymsWithInputGen: Gen[(Synonyms, List[Synonym], List[Synonym], List[Translation])] =
    for {
      translations <- Gen.listOf(dbGens.translation)
      synonyms1 <- dbGens.synonyms(translations.map(_.left))
      synonyms2 <- dbGens.synonyms(translations.map(_.right))
    } yield (Synonyms.from(translations, synonyms1, synonyms2), synonyms1, synonyms2, translations)

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

    "a single translation and a single synonym" in {
      assert(Synonyms.from(
        Translation("verraten", "trahir") :: Nil,
        Synonym("betrügen", "verraten") :: Nil,
        Nil) === Synonyms(
        Map(Vocabulary("betrügen") -> Set(Vocabulary("verraten")),
          Vocabulary("verraten") -> Set(Vocabulary("betrügen"))), Map.empty)
      )
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
            c <- syns(b) if c != a && !syns(c).contains(a)
          } yield (a, b, c)

          assert(problems.isEmpty)
        }
      }
    }

    "if a and b have the same translation, they are synonyms" in {
      forAll(synonymsWithTranslationsGen) { case (Synonyms(synsLeft, synsRight), translations) =>
        def synGroups(left: Translation => Vocabulary, right: Translation => Vocabulary) = {
          translations.groupBy(right).values
            .map(_.map(left))
            .collect { case w1 :: w2 :: ws => w1 -> NonEmptyList(w2, ws) }
            .map { case (w, ws) => ws.flatMap(w2 => NonEmptyList.of(Synonym(w, w2), Synonym(w2, w))) }
            .flatMap(_.toList)
            .toSet
        }

        val synGroupsLeft = synGroups(_.left, _.right)
        val synGroupsRight = synGroups(_.right, _.left)

        val missingSynsLeft = synGroupsLeft.filterNot { case Synonym(w1, w2) =>
          synsLeft(w1).contains(w2)
        }

        val missingSynsRight = synGroupsRight.filterNot { case Synonym(w1, w2) =>
          synsRight(w1).contains(w2)
        }

        assert(missingSynsLeft.isEmpty)
        assert(missingSynsRight.isEmpty)
      }
    }

    "existing synonyms are kept (except for synonyms of a word with itself)" in {
      forAll(synonymsWithInputGen) { case (Synonyms(synsLeft, synsRight), synsInLeft, synsInRight, _) =>
        def findMissingSynonyms(syns: Map[Vocabulary, Set[Vocabulary]], synsIn: List[Synonym]): List[Synonym] = {
          synsIn
            .filter(s => s.voc1 != s.voc2)
            .filterNot(s => syns.getOrElse(s.voc1, Set.empty).contains(s.voc2))
        }

        val missingSynsLeft = findMissingSynonyms(synsLeft, synsInLeft)
        val missingSynsRight = findMissingSynonyms(synsRight, synsInRight)
        assert(missingSynsLeft.isEmpty)
        assert(missingSynsRight.isEmpty)
      }
    }

    "A vocabulary is never a synonym to itself" in {
      forAll(synonymsGen) { syns =>
        def findSynsToSelf(map: Map[Vocabulary, Set[Vocabulary]]): Set[Vocabulary] = {
          map.collect { case (voc, vocs) if vocs.contains(voc) => voc }.toSet
        }

        assert(findSynsToSelf(syns.left).isEmpty)
        assert(findSynsToSelf(syns.right).isEmpty)
      }
    }
  }
}
