package ceedubs.irrec
package regex

import gen.RegexEq
import gen.RegexGen._

import cats.Eq
import cats.implicits._
import cats.laws.discipline.AlternativeTests
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.rng.Seed
import org.scalacheck.Gen.Parameters

class RegexTests extends IrrecSuite with Checkers {
  // This does really funky stuff.  Part of it is because we are defining equality for regular
  // expressions in a somewhat sketchy way.  Part of it is because Scalatest was throwing a
  // "java.lang.InternalError: Malformed class name" exception when I was using `check` inside a
  // `forAll`.
  test("Regex Alternative laws") {
    val gen = RegexEq.genRegexEq[Char, Match[Char]](5)
    val seed = Seed.random()
    val rEqv = gen(Parameters.default, seed).get // YOLO
    implicit def mkRegexEq[Out](implicit eqOut: Eq[Out]): Eq[RegexC[Out]] = rEqv(eqOut)
    val ruleSet = AlternativeTests[RegexC].alternative[Int, Int, Int]
    ruleSet.all.properties.foreach {
      case (id, prop) =>
        val clue = s"$id (seed: $seed)"
        withClue(clue)(check(prop))
    }
  }
}
