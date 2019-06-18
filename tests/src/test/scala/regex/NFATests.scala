package ceedubs.irrec
package regex

import RegexAndCandidate._

class NFATests extends IrrecSuite {
  test("runNFA and runNFAShortInput consistency") {
    forAll { rc: RegexAndCandidate[Int] =>
      val nfa = Glushkov.kleeneToNFA(rc.r)
      val matches: (Match[Int], Int) => Boolean = _.matches(_)
      val short = NFA.runNFAShortInput[Stream, Int, Match[Int], Int](nfa, matches)
      val normal = NFA.runNFA[Stream, Int, Match[Int], Int](nfa, matches)
      short(rc.candidate) should ===(normal(rc.candidate))
    }
  }
}
