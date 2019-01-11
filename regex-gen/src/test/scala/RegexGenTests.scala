package ceedubs.irrec
package regex

import cats.tests.CatsSuite
import org.scalacheck.Arbitrary.arbitrary

class RegexGenTests extends CatsSuite {
  test("regexMatchingStringGen generates a failing stream for Zero"){
    val r: Regex[Char] = Regex.impossible
    val gen = RegexGen.regexMatchingStreamGen(arbitrary[Char]).apply(r)
    gen.sample should ===(None)
  }
}
