package ceedubs.irrec
package regex

import CharRegexGen._
import org.scalacheck.Arbitrary.arbitrary
import ceedubs.irrec.parse.{regex => parse}
import NoShrink.noShrink

class CharRegexGenTests extends IrrecSuite {
  test("regexMatchingStandardStreamGen supports negated character classes") {
    val r2 = parse("[^ac-d]")
    val gen = for {
      r1 <- arbitrary[Regex[Char]]
      r3 <- arbitrary[Regex[Char]]
      r = r1 * r2 * r3
      s <- regexMatchingStringGen.apply(r)
    } yield (r, s)

    forAll(noShrink(gen)) {
      case NoShrink((r, s)) =>
        withClue(s"for input string <$s> and input regex <${r.pprint}>") {
          r.stringMatcher(s) should ===(true)
        }
    }
  }

  test("regexMatchingStringGen is consistent with RegexMatchingStreamGen") {
    val gen = for {
      r <- arbitrary[Regex[Char]]
      s <- regexMatchingStringGen(r)
    } yield (r, s)

    forAll(gen) {
      case (r, s) =>
        r.stringMatcher(s) should ===(true)
    }
  }
}
