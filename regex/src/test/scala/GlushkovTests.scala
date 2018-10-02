package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Regex._
import ceedubs.irrec.regex.RegexGen._
import qq.droste.scheme
import qq.droste.data.prelude._
import org.scalacheck.Gen
import RegexAndCandidate._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary

class GlushkovTests extends IrrecSuite {

  test("literal match"){assert(literal('b').stringMatcher("b"))}

  test("literal non-match"){assert(!(literal('b').stringMatcher("a")))}

  test("literal with trailing"){assert(!literal('b').stringMatcher("ba"))}

  test("or left match"){assert(or(literal('b'), literal('c')).stringMatcher("b"))}

  test("or left match with trailing"){assert(!or(literal('b'), literal('c')).stringMatcher("bc"))}

  test("or right match"){assert(or(literal('b'), literal('c')).stringMatcher("c"))}

  test("or right match with trailing"){assert(!or(literal('b'), literal('c')).stringMatcher("cb"))}

  test("or no match"){assert(!or(literal('b'), literal('c')).stringMatcher("a"))}

  test("or no match with trailing"){assert(!or(literal('b'), literal('c')).stringMatcher("ad"))}

  test("andThen match"){assert(andThen(literal('b'), literal('c')).stringMatcher("bc"))}

  test("andThen left only"){assert(!andThen(literal('b'), literal('c')).stringMatcher("bd"))}

  test("andThen right only"){assert(!andThen(literal('b'), literal('c')).stringMatcher("ac"))}

  test("andThen with trailing"){assert(!andThen(literal('b'), literal('c')).stringMatcher("bcd"))}

  test("star zero"){assert(star(literal('b')).stringMatcher(""))}

  test("star one"){assert(star(literal('b')).stringMatcher("b"))}

  test("star two"){assert(star(literal('b')).stringMatcher("bb"))}

  test("star three"){assert(star(or(literal('b'), literal('c'))).stringMatcher("bcb"))}

  test("star trailing"){assert(!star(or(literal('b'), literal('c'))).stringMatcher("bcbd"))}

  test("wildcard"){assert(wildcard[Char].stringMatcher("b"))}

  test("wildcard trailing"){assert(!wildcard[Char].stringMatcher("bc"))}

  test("wildcard empty"){assert(!wildcard[Char].stringMatcher(""))}

  test("inside range"){assert(range('a', 'c').stringMatcher("b"))}

  test("left range"){assert(range('a', 'c').stringMatcher("a"))}

  test("right range"){assert(range('a', 'c').stringMatcher("c"))}

  test("outside range"){assert(!range('a', 'c').stringMatcher("d"))}

  test("oneOrMore zero"){assert(!oneOrMore(literal('b')).stringMatcher(""))}

  test("oneOrMore one"){assert(oneOrMore(literal('b')).stringMatcher("b"))}

  test("oneOrMore two"){assert(oneOrMore(literal('b')).stringMatcher("bb"))}

  test("oneOrMore three"){assert(oneOrMore(literal('b')).stringMatcher("bbb"))}

  test("count zero empty"){assert(count(0, literal('b')).stringMatcher(""))}

  test("count zero non-empty"){assert(!count(0, literal('b')).stringMatcher("b"))}

  test("count 1 empty"){assert(!count(1, literal('b')).stringMatcher(""))}

  test("count 1 match"){assert(count(1, literal('b')).stringMatcher("b"))}

  test("count 1 non-match"){assert(!count(1, literal('b')).stringMatcher("c"))}

  test("count 2 match"){assert(count(2, literal('b')).stringMatcher("bb"))}

  test("count 2 non-match"){assert(!count(2, literal('b')).stringMatcher("bc"))}

  test("repeat examples"){
    val r = lit('b').repeat(2, 4)
    val m = r.stringMatcher
    m("") should ===(false)
    m("b") should ===(false)
    m("bb") should ===(true)
    m("bbb") should ===(true)
    m("bbbb") should ===(true)
    m("bbbbb") should ===(false)
    m("bcb") should ===(false)
  }

  test("repeat(0, n) matches empty"){
    forAll(arbitrary[Regex[Int]], Gen.chooseNum(0, 20)){ (r, max) =>
      assert(r.repeat(0, max).matcher[List].apply(List.empty))
    }
  }

  test("repeat(0, 0) doesn't match non-empty"){
    forAll(arbitrary[Regex[Int]], Gen.nonEmptyListOf(arbitrary[Int])){ (r, c) =>
      assert(!r.repeat(0, 0).matcher[List].apply(c))
    }
  }

  test("general regex matching"){
    forAll(genRegexAndMatch(true, arbitrary[Int])) { rm =>
      assert(rm.r.matcher[Stream].apply(rm.candidate))
    }
  }

  test("or(r, r) is equivalent to r"){
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(rc.r, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(impossible, r) is equivalent to r"){
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(impossible, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(r, impossible) is equivalent to r"){
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(rc.r, impossible)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("andThen(empty, r) is equivalent to r"){
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = andThen(Regex.empty, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("andThen(r, empty) is equivalent to r"){
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = andThen(rc.r, Regex.empty)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("if r matches, oneOrMore(r) matches"){
    forAll(genRegexAndMatch(true, arbitrary[Int])) { rc =>
      assert(oneOrMore(rc.r).matcher[Stream].apply(rc.candidate))
    }
  }

  test("if r matches x, oneOrMore(r) matches n * x"){
    forAll(genRegexAndMatch[Int](true, arbitrary[Int]), Gen.chooseNum(1, 10)){ (rc, n) =>
      oneOrMore(rc.r).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("if r matches x, star(r) matches n * x"){
    forAll(genRegexAndMatch(true, arbitrary[Int]), Gen.chooseNum(0, 10)){ (rc, n) =>
      star(rc.r).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("repeat(n, n, r) is equivalent to count(n, r)"){
    forAll(arbitrary[RegexAndCandidate[Int]], Gen.chooseNum(1, 10)){ (rc, n) =>
      val expected = rc.r.count(n).matcher[Stream].apply(rc.candidate)
      val equivR = rc.r.repeat(n, n)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("repeat(n, m, r) matches r.count(n) * r.star"){
    val gen = for {
      min <- Gen.chooseNum(0, 10)
      plus <- Gen.chooseNum(0, 5)
      r <- genRegex(arbitrary[Int], includeZero = false, includeOne = true)
      rRepeat = r.repeat(min, min + plus)
      //c <- rRepeat(regexMatchingStreamGen(arbitrary[Int]))
      c <- scheme.cata(regexMatchingStreamGen(arbitrary[Int])).apply(rRepeat)
    } yield (min, r, c)

    forAll(gen){ case (min, r, c) =>
      val r2 = r.count(min) * r.star
      assert(r2.matcher[Stream].apply(c))
    }
  }
}
