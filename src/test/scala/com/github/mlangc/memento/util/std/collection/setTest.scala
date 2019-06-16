package com.github.mlangc.memento.util.std.collection

import com.github.mlangc.memento.BaseTest
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.github.mlangc.memento.util.std.collection.set._
import org.scalacheck.Gen
import org.scalatest.Matchers

class setTest extends BaseTest with ScalaCheckPropertyChecks with Matchers {
  "connectedSets with" - {
    "trivial input" in {
      Set[Int]().connectedSets((_, _) => false) should be (Set())
      Set(1).connectedSets((_, _) => true) should be (Set(Set(1)))
    }

    "a trivial graphs" in {
      Set(1, 2, 3).connectedSets((_, _) => true) should be (Set(Set(1, 2, 3)))
      Set(1, 2, 3).connectedSets((_, _) => false) should be (Set(Set(1), Set(2), Set(3)))
    }

    "a small example" in {
      /*
       * 1 - 2 - 3 ------- 8
       *      \            |
       *       4 - 5 - 6 - 7
       *
       * 9 - 10 - 11
       *
       * 12 - 13
       *  \
       *   14
       *
       * 15
       */
      def edgeExists(v1: Int, v2: Int) = v1 match {
        case 1 => v2 == 2
        case 2 => Set(1, 3, 4).contains(v2)
        case 3 => Set(3, 8).contains(v2)
        case 4 => Set(2, 5).contains(v2)
        case 5 => Set(4, 6).contains(v2)
        case 6 => Set(5, 7).contains(v2)
        case 7 => Set(6, 8).contains(v2)
        case 8 => Set(3, 7).contains(v2)
        case 9 => v2 == 10
        case 10 => Set(9, 11).contains(v2)
        case 11 => v2 == 10
        case 12 => Set(13, 14).contains(v2)
        case 13 => v2 == 12
        case 14 => v2 == 12
        case _ => false
      }

      (1.to(15).toSet).connectedSets(edgeExists) should be (Set(Set(1, 2, 3, 4, 5, 6, 7, 8), Set(9, 10, 11), Set(12, 13, 14), Set(15)))
    }

    "random noise" in {
      val genInput = for {
        n <- Gen.choose(0, 10*100)
        k <- Gen.choose(1, 100)
      } yield (n, k)

      forAll(genInput) { case (n, k) =>
        if (n >= 0 && k >= 1) {
          def edgeExists(i1: Int, i2: Int) = {
            i1 < i2 && (i1 - i2) % k == 0
          }

          val objs = 0.until(n).toSet
          val connObjs = objs.connectedSets(edgeExists)
          val allConnObjs = connObjs.flatten
          val missing = objs.diff(allConnObjs)
          missing should be (empty)
        }
      }
    }
  }

}
