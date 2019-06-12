package ceedubs.irrec
package regex

import Regex._
import RegexAndCandidate._

class KleeneOptimizationTests extends IrrecSuite {
  test("an optimized regular expression still produces the same result") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val actual = rc.r.optimize.matcher[Stream].apply(rc.candidate)
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("optimization changes (empty|r)* to empty|r*") {
    val r = (Regex.empty | lit('b')).star
    val optimizedR = r.optimize
    optimizedR should be(Regex.empty | lit('b').star)
  }

  test("optimization changes (r|empty)* to r*|empty") {
    val r = (lit('b') | Regex.empty).star
    val optimizedR = r.optimize
    optimizedR should be(lit('b').star | Regex.empty)
  }
}
