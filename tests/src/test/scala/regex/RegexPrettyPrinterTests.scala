package ceedubs.irrec
package regex

import ceedubs.irrec.parse.{regex => parse}
import Regex._

class RegexPrettyPrinterTests extends IrrecSuite {
  test("char regex pretty print examples") {
    lit('a').pprint should ===("a")
    lit('a').star.pprint should ===("a*")
    lit('a').star.star.pprint should ===("(?:a*)*")
    lit('a').star.star.star.pprint should ===("(?:(?:a*)*)*")
    (lit('a') | lit('b')).pprint should ===("a|b")
    (lit('a') | lit('b')).star.pprint should ===("(?:a|b)*")
    (lit('a') * lit('b') * (lit('c') | lit('d')).star).pprint should ===("ab(?:c|d)*")
  }

  test(
    "char regex pretty print shouldn't escape characters that don't need it in character classes") {
    range('!', '*').pprint should ===("[!-*]")
  }

  test(
    "char regex pretty print shouldn't escape characters that don't need it in negative character classes") {
    parse("[^!*]").pprint should ===("[^!*]")
  }

  test("char regex pretty print should handle Zero") {
    (lit('a') * impossible * lit('b')).pprint should ===("a∅b")
  }

  test("char regex pretty print should handle One") {
    (lit('a') * Regex.empty * lit('b')).pprint should ===("ab")
  }

  test("char regex pretty print should display non-graphical characters as unicode code points") {
    lit(' ').pprint should ===("\\u0020")
  }

  test(
    "char regex pretty print should display non-graphical characters in character classes as unicode code points") {
    range('\u0000', ' ').pprint should ===("[\\u0000-\\u0020]")
  }

  test("TODO ceedubs 1"){
    import applicative.RE
    import CharRegexGen._
    forAll { r0: Regex[Char] =>
      val r = r0.optimize
      applicative.RegexPrettyPrinter.pprintRE(RE.ofRegex(r)) should ===(r.pprint)
    }
  }

  // TODO ceedubs should I print as named capturing groups?
  // TODO ceedubs I should probably be passing an Option[Int] to captureAs?
  //test("char regex pretty print capturing examples") {
  //  lit('a').captureAs(1).pprint should ===("(a)")
  //  lit('a').star.captureAs(1).pprint should ===("(a*)")
  //  lit('a').star.star.captureAs(1).pprint should ===("((?:a*)*)")
  //  lit('a').star.star.star.captureAs(1).pprint should ===("((?:(?:a*)*)*)")
  //  (lit('a').captureAs(1) | lit('b').captureAs(1)).pprint should ===("(a)|(b)")
  //  (lit('a') | lit('b')).star.pprint should ===("(?:a|b)*")
  //  (lit('a') * lit('b') * (lit('c') | lit('d')).star).pprint should ===("ab(?:c|d)*")
  //}
}
