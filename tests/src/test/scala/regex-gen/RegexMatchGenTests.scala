package ceedubs.irrec
package regex

import RegexGen._
import RegexMatchGen._

import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

class RegexMatchGenTests extends IrrecSuite {
  test("regexMatchingStreamGen generates a failing stream for fail") {
    val r: RegexC[Long] = combinator.fail
    val gen = regexMatchingStreamGen[Char](_ => Gen.const('a')).apply(r)
    gen.sample should ===(None)
  }

  test("genRegexMatch Byte works") {
    val gen = for {
      r <- arbitrary[RegexM[Byte, Long]]
      c <- genRegexMatch(r)
    } yield (r, c)

    forAll(gen) {
      case (r, c) =>
        r.matcher[Stream].apply(c) should ===(true)
    }
  }

  test("genRegexMatch Long works") {
    val gen = for {
      r <- arbitrary[RegexM[Long, Long]]
      c <- genRegexMatch(r)
    } yield (r, c)

    forAll(gen) {
      case (r, c) =>
        r.matcher[Stream].apply(c) should ===(true)
    }
  }
}
