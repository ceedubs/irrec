package ceedubs.irrec
package regex

import Regex._
import org.scalacheck.Arbitrary.arbitrary
import RegexAndCandidate._

class RegexPrettyPrinterTests extends IrrecSuite {
  test("char regex pretty printer matches java Pattern"){
    forAll(genRegexAndMatch(false, arbitrary[Char])){ rm =>
      val asString = rm.r.pprint
      val javaR = java.util.regex.Pattern.compile(asString)
      assert(javaR.matcher(rm.candidate.mkString).matches, s"${rm.candidate.toList} should match $asString")
    }
  }

  test("char regex pretty print examples"){
    lit('a').pprint should ===("a")
    lit('a').star.pprint should ===("a*")
    lit('a').star.star.pprint should ===("(a*)*")
    lit('a').star.star.star.pprint should ===("((a*)*)*")
    (lit('a') | lit('b')).pprint should ===("a|b")
    (lit('a') | lit('b')).star.pprint should ===("(a|b)*")
    (lit('a') * lit('b') * (lit('c') | lit('d')).star).pprint should ===("ab(c|d)*")
  }
}
