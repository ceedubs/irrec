package ceedubs.irrec
package regex

import RegexGen._
import CharRegexGen._
import ceedubs.irrec.parse.{regex => parse}
import NoShrink.noShrink

import cats.implicits._
import org.scalacheck.Arbitrary.arbitrary

class CharRegexGenTests extends IrrecSuite {
  test("regexMatchingStandardStreamGen supports negated character classes") {
    val r2 = parse("[^ac-d]")
    val gen = for {
      r1 <- arbitrary[RegexC[Long]]
      r3 <- arbitrary[RegexC[Double]]
      r = r1.void <* r2 <* r3
      s <- genRegexMatchingString.apply(r)
    } yield (r, s)

    forAll(noShrink(gen)) {
      case NoShrink((r, s)) =>
        withClue(s"for input string <$s> and input regex <${r.pprint}>") {
          r.stringMatcher(s) should ===(true)
        }
    }
  }

  test("genRegexMatchingString is consistent with RegexMatchingStreamGen") {
    val gen = for {
      r <- arbitrary[RegexC[Long]]
      s <- genRegexMatchingString(r)
    } yield (r, s)

    forAll(gen) {
      case (r, s) =>
        r.stringMatcher(s) should ===(true)
    }
  }
}
