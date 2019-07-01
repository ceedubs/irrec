package ceedubs.irrec
package regex

import Match._

import cats.collections.Diet

// TODO ceedubs should be testing these with Diet gen instead of individual values
class MatchTests extends IrrecSuite {
  // TODO ceedubs is this a valid case? We don't really want one of these to exist.
  // does Ior solve the problem?
  // probably not since Diet can be empty
  test("empty matchset matches everything"){
    forAll { c: Char =>
      MatchSet.forbid(Diet.empty).matches(c) should ===(true)
    }
  }

  test("TODO ceedubs 1"){
    forAll { (x: Char, y: Char) =>
      MatchSet.allow(Diet.one(x)).matches(y) should ===(x == y)
    }
  }

  test("TODO ceedubs 2"){
    forAll { (x: Char) =>
      val pos = MatchSet.allow(Diet.one(x))
      val neg = MatchSet.forbid(Diet.one(x))

      (pos union neg).matches(x) should ===(true)
      (pos intersect neg).matches(x) should ===(false)
    }
  }

  test("TODO ceedubs 3"){
    forAll { (x: Char, y: Char) =>
      val m1 = MatchSet.forbid(Diet.one(x))
      val m2 = MatchSet.forbid(Diet.one(y))

      (m1 union m2).matches(x) should ===(x != y)
      (m1 union m2).matches(y) should ===(x != y)
      (m1 intersect m2).matches(x) should ===(false)
      (m1 intersect m2).matches(y) should ===(false)
    }
  }

  test("TODO ceedubs 4"){
    forAll { (x: Char) =>
      val m1 = MatchSet.allow(CharacterClasses.whitespaceChar)
      val m2 = MatchSet.forbid(CharacterClasses.whitespaceChar)

      (m1 intersect m2).matches(x) should ===(false)
    }
  }
}
