package ceedubs.irrec
package regex

import RegexGen._
import NoShrink.noShrink

import cats.data.NonEmptyList
import cats.tests.CatsSuite
import org.scalacheck.Arbitrary.arbitrary
import qq.droste.data.Coattr

class RegexGenTests extends CatsSuite {
  test("regexMatchingStreamGen generates a failing stream for Zero") {
    val r: Regex[Char] = Regex.impossible
    val gen = RegexGen.regexMatchingStreamGen(arbitrary[Char]).apply(r)
    gen.sample should ===(None)
  }

  // Currently the regex generators aren't generating negative character classes, because the
  // generators for steams matching a regex implement negative character classes using the `filter`
  // method on `Gen`, and this could lead to Scalacheck generating a lot of values that end up
  // needing to be filtered; potentially until it gives up. So for now we'll test a small case where
  // Scalacheck getting exhausted is unlikely.
  test("regexMatchingStreamGen supports negated character classes") {
    import Match._, Negated._
    val r2: Regex[Char] = Coattr.pure(
      NoneOf(NonEmptyList.of(NegatedLiteral(Literal('a')), NegatedRange(Range('c', 'd')))))
    val gen = for {
      r1 <- arbitrary[Regex[Char]]
      r3 <- arbitrary[Regex[Char]]
      r = r1 * r2 * r3
      s <- RegexGen.regexMatchingStringGen(arbitrary[Char]).apply(r)
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
      s <- RegexGen.regexMatchingStringGen(arbitrary[Char]).apply(r)
    } yield (r, s)

    forAll(gen) {
      case (r, s) =>
        r.stringMatcher(s) should ===(true)
    }
  }
}
