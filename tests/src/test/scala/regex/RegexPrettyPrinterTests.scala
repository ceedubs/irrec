package ceedubs.irrec
package regex

import ceedubs.irrec.parse.{regex => parse}
import Regex._

class RegexPrettyPrinterTests extends IrrecSuite {

  test("char regex pretty print examples") {
    lit('a').pprint should ===("a")
    lit('a').star.pprint should ===("a*")
    lit('a').star.star.pprint should ===("(a*)*")
    lit('a').star.star.star.pprint should ===("((a*)*)*")
    (lit('a') | lit('b')).pprint should ===("a|b")
    (lit('a') | lit('b')).star.pprint should ===("(a|b)*")
    (lit('a') * lit('b') * (lit('c') | lit('d')).star).pprint should ===("ab(c|d)*")
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
}
