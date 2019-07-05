package ceedubs.irrec
package regex

import Match._

import cats.implicits._
import cats.collections.Diet
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

class MatchTests extends IrrecSuite {
  import MatchTests._

  test("empty matchset matches everything") {
    forAll { c: Char =>
      MatchSet.forbid(Diet.empty).matches(c) should ===(true)
    }
  }

  test("allowing a single item only matches that item") {
    forAll { (x: Char, y: Char) =>
      MatchSet.allow(Diet.one(x)).matches(y) should ===(x == y)
    }
  }

  test("allow(x) union forbid(x) should allow everything") {
    forAll { (x: Char) =>
      val pos = MatchSet.allow(Diet.one(x))
      val neg = MatchSet.forbid(Diet.one(x))

      (pos union neg).matches(x) should ===(true)
    }
  }

  test("allow(x) intersect forbid(x) shouldn't allow anything") {
    forAll(charDietGen, arbitrary[Char]) { (x, y) =>
      val pos = MatchSet.allow(x)
      val neg = MatchSet.forbid(x)

      (pos intersect neg).matches(y) should ===(false)
    }
  }

  test("[^x][^y] should match iff x and y are equal and input is x") {
    forAll { (x: Char, y: Char) =>
      val m1 = MatchSet.forbid(Diet.one(x))
      val m2 = MatchSet.forbid(Diet.one(y))

      (m1 union m2).matches(x) should ===(x != y)
    }
  }

  test("forbid intersection") {
    forAll { (x: Char, y: Char) =>
      val m1 = MatchSet.forbid(Diet.one(x))
      val m2 = MatchSet.forbid(Diet.one(y))

      (m1 intersect m2).matches(x) should ===(false)
      (m1 intersect m2).matches(y) should ===(false)
    }
  }
}

object MatchTests {
  val charDietGen: Gen[Diet[Char]] = DietGen.genDiscreteDiet(arbitrary[Char])
}
