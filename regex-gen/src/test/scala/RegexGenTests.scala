package ceedubs.irrec
package regex

import RegexGen._

import cats.tests.CatsSuite
import org.scalacheck.Arbitrary.arbitrary

class RegexGenTests extends CatsSuite {
  test("regexMatchingStreamGen generates a failing stream for Zero"){
    val r: Regex[Char] = Regex.impossible
    val gen = RegexGen.regexMatchingStreamGen(arbitrary[Char]).apply(r)
    gen.sample should ===(None)
  }

  test("regexMatchingStringGen is consistent with RegexMatchingStreamGen"){
    val gen = for {
      r <- arbitrary[Regex[Char]]
      s <- RegexGen.regexMatchingStringGen(arbitrary[Char]).apply(r)
    } yield (r, s)

    forAll(gen){ case (r, s) =>
      r.stringMatcher(s) should ===(true)
    }
  }
}
