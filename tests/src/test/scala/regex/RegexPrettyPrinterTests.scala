package ceedubs.irrec
package regex

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

  test("char regex pretty print should handle Zero") {
    (lit('a') * impossible * lit('b')).pprint should ===("a∅b")
  }

  test("char regex pretty print should handle One") {
    (lit('a') * Regex.empty * lit('b')).pprint should ===("ab")
  }
}
