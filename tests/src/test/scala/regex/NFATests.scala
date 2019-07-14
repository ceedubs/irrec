package ceedubs.irrec
package regex

import RegexAndCandidate._
import RegexPrettyPrinter.showCharMatch
import parse.{regex => parse}

import cats.Id
import cats.data.{Chain, Writer}

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

  test("runNFA and runNFAWithEffect consistency") {
    forAll { rc: RegexAndCandidate[Int] =>
      val nfa = Glushkov.kleeneToNFA(rc.r)
      val matches: (Match[Int], Int) => Boolean = _.matches(_)
      val withEffect =
        NFA.runNFAWithEffect[Stream, Id, Int, Match[Int], Int](nfa, (_, _, b, a) => matches(b, a))
      val normal = NFA.runNFA[Stream, Int, Match[Int], Int](nfa, matches)
      withEffect(rc.candidate) should ===(normal(rc.candidate))
    }
  }

  test("runNFAWithEffect Writer") {
    val r = parse("ab(c|d)e")
    val nfa = Glushkov.kleeneToNFA(r)
    val withEffect = NFA.runNFAWithEffect[Stream, Writer[Chain[String], ?], Int, Match[Char], Char](
      nfa,
      (n1, n2, m, c) =>
        Writer(Chain.one(s"n1: $n1, n2: $n2, m: ${showCharMatch(m)}, c: $c"), m.matches(c)))
    val expectedLog = Chain(
      "n1: 0, n2: 1, m: a, c: a",
      "n1: 1, n2: 2, m: b, c: b",
      "n1: 2, n2: 3, m: c, c: c",
      "n1: 2, n2: 4, m: d, c: c")
    val expected = Writer(expectedLog, false)
    withEffect("abc".toStream) should ===(expected)
  }
}
