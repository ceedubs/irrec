package ceedubs.irrec
package regex

import RegexAndCandidate._

class KleeneOptimizationTests extends IrrecSuite {
  test("char regex pretty printer matches java Pattern"){
    forAll{ (rc: RegexAndCandidate[Int]) =>
      val actual = rc.r.optimize.matcher[Stream].apply(rc.candidate)
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }
}
