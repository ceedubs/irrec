package ceedubs.irrec
package regex

import gen.RegexEq._
import gen.RegexGen._
import gen.RegexMatchGen.genRegexMatch

import cats.kernel.Eq
import algebra.laws.RingLaws
import org.scalacheck.Prop
import org.scalacheck.Arbitrary.arbitrary
import cats.implicits._
import cats.laws.discipline.{AlternativeTests, FunctorFilterTests}
import org.scalatestplus.scalacheck.Checkers
import ceedubs.irrec.regex.gen.RegexAndCandidate

class RegexTests extends IrrecSuite with Checkers {
  test("Regex Alternative laws") {
    val prop = Prop.forAll(genRegexEqK[Char, Match[Char]](5)) { rEqv =>
      implicit def mkRegexEq[Out](implicit eqOut: Eq[Out]): Eq[RegexC[Out]] = rEqv(eqOut)
      rulesetToProp(AlternativeTests[RegexC].alternative[Int, Int, Int])
    }
    check(prop)
  }

  test("Regex FunctorFilter laws") {
    val prop = Prop.forAll(genRegexEqK[Char, Match[Char]](5)) { rEqv =>
      implicit def mkRegexEq[Out](implicit eqOut: Eq[Out]): Eq[RegexC[Out]] = rEqv(eqOut)
      rulesetToProp(FunctorFilterTests[RegexC].functorFilter[Int, Int, Int])
    }
    check(prop)
  }

  test("r1.mapFilter(_ => None) | r2 should be equivalent to r2") {
    forAll { (rc1: RegexAndCandidate[Int, Long], r2: RegexM[Int, String]) =>
      (rc1.r.mapFilter[String](_ => None) | r2).compile.parseOnly(rc1.candidate) should ===(
        r2.compile.parseOnly(rc1.candidate))
    }
  }

  test("r1.mapFilter(_ => None) *> r2 should never match") {
    val gen = for {
      rc1 <- arbitrary[RegexAndCandidate[Int, Long]]
      r2 <- arbitrary[RegexM[Int, String]]
      match2 <- genRegexMatch(r2)
    } yield (rc1, r2, match2)
    forAll(gen) { case (rc1, r2, match2) =>
      val input = rc1.candidate ++ match2
      (rc1.r.mapFilter(_ => None) *> r2).compile.parseOnly(input) should ===(None)
    }
  }

  test("Regex Rig laws") {
    val prop = Prop.forAll(genRegexEq[Char, Match[Char], Unit](5)) { implicit regexEq =>
      rulesetToProp(RingLaws[RegexC[Unit]].rig)
    }
    check(prop)
  }
}
