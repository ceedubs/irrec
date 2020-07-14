package ceedubs.irrec
package regex

import gen.RegexEq._
import gen.RegexGen._

import cats.kernel.Eq
import algebra.laws.RingLaws
import org.scalacheck.Prop
import cats.implicits._
import cats.laws.discipline.AlternativeTests
import org.scalatestplus.scalacheck.Checkers

class RegexTests extends IrrecSuite with Checkers {
  test("Regex Alternative laws") {
    val prop = Prop.forAll(genRegexEqK[Char, Match[Char]](5)) { rEqv =>
      implicit def mkRegexEq[Out](implicit eqOut: Eq[Out]): Eq[RegexC[Out]] = rEqv(eqOut)
      rulesetToProp(AlternativeTests[RegexC].alternative[Int, Int, Int])
    }
    check(prop)
  }

  test("Regex Rig laws") {
    val prop = Prop.forAll(genRegexEq[Char, Match[Char], Unit](5)) { implicit regexEq =>
      rulesetToProp(RingLaws[RegexC[Unit]].rig)
    }
    check(prop)
  }
}
