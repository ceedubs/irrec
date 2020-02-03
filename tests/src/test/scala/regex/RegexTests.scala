//package ceedubs.irrec
//package regex
//
//import gen.RegexEq
//import gen.RegexGen._
//
//import cats.Eq
//import cats.implicits._
//import cats.laws.discipline.AlternativeTests
//import org.scalatestplus.scalacheck.Checkers
//
//class RegexTests extends IrrecSuite with Checkers {
//  test("Regex Alternative laws") {
//    val gen = RegexEq.genRegexEq[Char, Match[Char]](5)
//    forAll(gen, minSuccessful(1)) { rEqv =>
//      implicit def mkRegexEq[Out](implicit eqOut: Eq[Out]): Eq[RegexC[Out]] = rEqv(eqOut)
//      val ruleSet = AlternativeTests[RegexC].alternative[Int, Int, Int]
//      ruleSet.all.properties.foreach {
//        case (id, prop) =>
//          withClue(id)(check(prop))
//      }
//    }
//  }
//}
