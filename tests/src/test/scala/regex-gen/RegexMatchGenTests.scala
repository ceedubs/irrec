package ceedubs.irrec
package regex

import RegexMatchGen._
import CharRegexGen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import ceedubs.irrec.parse.{regex => parse}
import NoShrink.noShrink

// TODO ceedubs this actually tests methods from CharRegexGen?
class RegexMatchGenTests extends IrrecSuite {
  test("regexMatchingStreamGen generates a failing stream for Zero") {
    val r: Regex[Char] = Regex.impossible
    val gen = regexMatchingStreamGen[Char](_ => Gen.const('a')).apply(r)
    gen.sample should ===(None)
  }

  test("regexMatchingStreamGen supports negated character classes") {
    val r2 = parse("[^ac-d]")
    val gen = for {
      r1 <- arbitrary[Regex[Char]]
      r3 <- arbitrary[Regex[Char]]
      r = r1 * r2 * r3
      s <- CharRegexGen.regexMatchingStandardStringGen.apply(r)
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
      s <- CharRegexGen.regexMatchingStandardStringGen(r)
    } yield (r, s)

    forAll(gen) {
      case (r, s) =>
        r.stringMatcher(s) should ===(true)
    }
  }
}
