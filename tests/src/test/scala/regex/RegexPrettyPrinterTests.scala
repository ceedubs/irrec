package ceedubs.irrec
package regex

import ceedubs.irrec.parse.{regex => parse}
import combinator._
import Greediness._

import cats.implicits._

class RegexPrettyPrinterTests extends IrrecSuite {
  test("char regex pretty print examples") {
    lit('a').pprint should ===("a")
    lit('a').star(Greedy).pprint should ===("a*")
    lit('a').star(NonGreedy).pprint should ===("a*?")
    lit('a').*.*.pprint should ===("(a*)*")
    lit('a').*?.*?.pprint should ===("(a*?)*?")
    lit('a').*.*.*.pprint should ===("((a*)*)*")
    (lit('a') | lit('b')).pprint should ===("a|b")
    (lit('a') | lit('b')).*.pprint should ===("(a|b)*")
    (lit('a') <* lit('b') <* (lit('c') | lit('d')).*).pprint should ===("ab(c|d)*")
  }

  test("char regex repeat pretty print examples") {
    lit('a').repeat(1, None, Greedy).pprint should ===("a{1,}")
    lit('a').repeat(1, None, NonGreedy).pprint should ===("a{1,}?")
    lit('a').repeat(1, Some(3), Greedy).pprint should ===("a{1,3}")
    lit('a').repeat(1, Some(4), NonGreedy).pprint should ===("a{1,4}?")
    (lit('a') | lit('b')).repeat(1, Some(4), NonGreedy).pprint should ===("(a|b){1,4}?")
    (lit('a').void | lit('b').repeat(1, Some(4), NonGreedy).void).pprint should ===("a|b{1,4}?")
    lit('a').repeat(1, Some(4), NonGreedy).star(NonGreedy).pprint should ===("(a{1,4}?)*?")
    lit('a').repeat(1, Some(4), Greedy).star(NonGreedy).pprint should ===("(a{1,4})*?")
    lit('a').star(Greedy).repeat(1, Some(4), NonGreedy).star(NonGreedy).pprint should ===(
      "((a*){1,4}?)*?")
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
    (lit('a') <* combinator.fail <* lit('b')).pprint should ===("a∅b")
  }

  test("char regex pretty print should handle One") {
    (lit('a') <* combinator.empty <* lit('b')).pprint should ===("ab")
  }

  test("char regex pretty print should display non-graphical characters as unicode code points") {
    lit(' ').pprint should ===("\\u0020")
  }

  test(
    "char regex pretty print should display non-graphical characters in character classes as unicode code points") {
    range('\u0000', ' ').pprint should ===("[\\u0000-\\u0020]")
  }
}
