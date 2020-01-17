package ceedubs.irrec
package regex

import Regex.Regex
import RegexMatchGen._
import org.scalacheck.Gen

class RegexMatchGenTests extends IrrecSuite {
  test("regexMatchingStreamGen generates a failing stream for Zero") {
    val r: Regex[Char, Long] = Regex.fail
    val gen = regexMatchingStreamGen[Char](_ => Gen.const('a')).apply(r)
    gen.sample should ===(None)
  }
}
