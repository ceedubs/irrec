package ceedubs.irrec
package regex

import Regex._
import RegexAndCandidate._
import cats.collections.{Diet, Range}

class KleeneOptimizationTests extends IrrecSuite {
  test("an optimized regular expression still produces the same result") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val actual = rc.r.optimize.matcher[Stream].apply(rc.candidate)
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("optimization changes (empty|r)* r*|empty") {
    val r = (Regex.empty | lit('b')).star
    val optimizedR = r.optimize
    optimizedR should be(lit('b').star | Regex.empty)
  }

  test("optimization changes (r|empty)* to r*|empty") {
    val r = (lit('b') | Regex.empty).star
    val optimizedR = r.optimize
    optimizedR should be(lit('b').star | Regex.empty)
  }

  test("optimization changes a|b to [ab]") {
    val r = lit('a') | lit('b')
    val optimizedR = r.optimize
    optimizedR should be(range('a', 'b'))
  }

  test("optimization changes [ad]|b|f|[ez] to [a-bd-fz]") {
    val r = inSet(Diet.one('a') + 'd') | lit('b') | lit('f') | inSet(Diet.one('e') + 'z')
    val optimizedR = r.optimize
    val expected = inSet(Diet.fromRange(Range('a', 'b')) + Range('d', 'f') + 'z')
    optimizedR should be(expected)
  }
}
