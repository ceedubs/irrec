package ceedubs.irrec
package regex

import Regex._
import CharRegexGen._

class RegexPrettyPrinterTests extends IrrecSuite {

  test("char regex pretty printer matches java Pattern") {
    forAll(genCharRegexAndMatch) { rm =>
      val prettyRegex = rm.r.pprint
      val regexHex = prettyRegex.map(_.toInt.toHexString).toList
      val javaR = rm.r.toPattern
      val candidateHex = rm.candidate.map(_.toInt.toHexString).toList
      assert(
        javaR.matcher(rm.candidate.mkString).matches,
        s"${rm.candidate.mkString} ($candidateHex) should match $prettyRegex ($regexHex)")
    }
  }

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
