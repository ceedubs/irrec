package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Regex._
import ceedubs.irrec.regex.RegexGen._
import org.scalacheck.Gen
import RegexAndCandidate._
import RegexMatchGen._
import org.scalacheck.Arbitrary, Arbitrary.arbitrary
import cats.data.NonEmptyList
import cats.laws.discipline.arbitrary._
import ceedubs.irrec.parse.{regex => parse}

class GlushkovTests extends IrrecSuite {
  test("literal match") { assert(literal('b').stringMatcher("b")) }

  test("literal non-match") { assert(!(literal('b').stringMatcher("a"))) }

  test("literal with trailing") { assert(!literal('b').stringMatcher("ba")) }

  test("or left match") { assert(or(literal('b'), literal('c')).stringMatcher("b")) }

  test("or left match with trailing") {
    assert(!or(literal('b'), literal('c')).stringMatcher("bc"))
  }

  test("or right match") { assert(or(literal('b'), literal('c')).stringMatcher("c")) }

  test("or right match with trailing") {
    assert(!or(literal('b'), literal('c')).stringMatcher("cb"))
  }

  test("or no match") { assert(!or(literal('b'), literal('c')).stringMatcher("a")) }

  test("or no match with trailing") { assert(!or(literal('b'), literal('c')).stringMatcher("ad")) }

  test("andThen match") { assert(andThen(literal('b'), literal('c')).stringMatcher("bc")) }

  test("andThen left only") { assert(!andThen(literal('b'), literal('c')).stringMatcher("bd")) }

  test("andThen right only") { assert(!andThen(literal('b'), literal('c')).stringMatcher("ac")) }

  test("andThen with trailing") {
    assert(!andThen(literal('b'), literal('c')).stringMatcher("bcd"))
  }

  test("star zero") { assert(star(literal('b')).stringMatcher("")) }

  test("star one") { assert(star(literal('b')).stringMatcher("b")) }

  test("star two") { assert(star(literal('b')).stringMatcher("bb")) }

  test("star three") { assert(star(or(literal('b'), literal('c'))).stringMatcher("bcb")) }

  test("star trailing") { assert(!star(or(literal('b'), literal('c'))).stringMatcher("bcbd")) }

  test("wildcard") { assert(wildcard[Char].stringMatcher("b")) }

  test("wildcard trailing") { assert(!wildcard[Char].stringMatcher("bc")) }

  test("wildcard empty") { assert(!wildcard[Char].stringMatcher("")) }

  test("inside range") { assert(range('a', 'c').stringMatcher("b")) }

  test("left range") { assert(range('a', 'c').stringMatcher("a")) }

  test("right range") { assert(range('a', 'c').stringMatcher("c")) }

  test("outside range") { assert(!range('a', 'c').stringMatcher("d")) }

  test("oneOrMore zero") { assert(!literal('b').oneOrMore.stringMatcher("")) }

  test("oneOrMore one") { assert(oneOrMore(literal('b')).stringMatcher("b")) }

  test("oneOrMore two") { assert(oneOrMore(literal('b')).stringMatcher("bb")) }

  test("oneOrMore three") { assert(oneOrMore(literal('b')).stringMatcher("bbb")) }

  test("count zero empty") { assert(count(0, literal('b')).stringMatcher("")) }

  test("count zero non-empty") { assert(!count(0, literal('b')).stringMatcher("b")) }

  test("count 1 empty") { assert(!count(1, literal('b')).stringMatcher("")) }

  test("count 1 match") { assert(count(1, literal('b')).stringMatcher("b")) }

  test("count 1 non-match") { assert(!count(1, literal('b')).stringMatcher("c")) }

  test("count 2 match") { assert(count(2, literal('b')).stringMatcher("bb")) }

  test("count 2 non-match") { assert(!count(2, literal('b')).stringMatcher("bc")) }

  test("oneOf first match") { assert(Regex.oneOf('a', 'b', 'c').stringMatcher("a")) }

  test("oneOf second match") { assert(Regex.oneOf('a', 'b', 'c').stringMatcher("b")) }

  test("oneOf last match") { assert(Regex.oneOf('a', 'b', 'c').stringMatcher("c")) }

  test("oneOf non match") { assert(!Regex.oneOf('a', 'b', 'c').stringMatcher("d")) }

  test("seq empty match") { assert(seq("").stringMatcher("")) }

  test("seq empty non-match") { assert(!seq("").stringMatcher("a")) }

  test("seq single match") { assert(seq("a").stringMatcher("a")) }

  test("seq match") { assert(seq("abc").stringMatcher("abc")) }

  test("seq non-match") { assert(!seq("abc").stringMatcher("bcd")) }

  test("optional match present") {
    assert((lit('a') * lit('b').optional * lit('c')).stringMatcher("abc"))
  }

  test("optional match not present") {
    assert((lit('a') * lit('b').optional * lit('c')).stringMatcher("ac"))
  }

  test("character class literal match beginning") {
    assert(parse("a[bd-fh]j").stringMatcher("abj"))
  }

  test("character class literal match middle") { assert(parse("a[bd-fhj]l").stringMatcher("ahl")) }

  test("character class literal match end") { assert(parse("a[bd-fh]j").stringMatcher("ahj")) }

  test("character class literal non-match") { assert(!parse("a[bd-fh]j").stringMatcher("axj")) }

  test("character class range match beginning") { assert(parse("a[d-fh]j").stringMatcher("aej")) }

  test("character class range match end") { assert(parse("a[bd-f]j").stringMatcher("aej")) }

  test("character class range non-match") { assert(!parse("a[d-fh]j").stringMatcher("axj")) }

  test("character class range match low") { assert(parse("a[bd-fh]j").stringMatcher("adj")) }

  test("character class range match high") { assert(parse("a[bd-fh]j").stringMatcher("afj")) }

  test("digit character single match") { assert(parse("\\d").stringMatcher("2")) }

  test("digit character non-match") { assert(!parse("\\d").stringMatcher("a")) }

  test("non-digit character single match") { assert(parse("\\D").stringMatcher("a")) }

  test("non-digit character non-match") { assert(!parse("\\D").stringMatcher("3")) }

  test("word character single match") { assert(parse("\\w").stringMatcher("a")) }

  test("word character non-match") { assert(!parse("\\w").stringMatcher("%")) }

  test("non-word character single match") { assert(parse("\\W").stringMatcher("%")) }

  test("non-word character non-match") { assert(!parse("\\W").stringMatcher("a")) }

  test("whitespace character single match") { assert(parse("\\s").stringMatcher(" ")) }

  test("whitespace character non-match") { assert(!parse("\\s").stringMatcher("%")) }

  test("whitespace character negated range match") {
    assert(parse("a[^\\s]c").stringMatcher("abc"))
  }

  test("whitespace character negated range non-match") {
    assert(!parse("a[^\\s]c").stringMatcher("a c"))
  }

  test("non-whitespace character single match") { assert(parse("\\S").stringMatcher("a")) }

  test("non-whitespace character non-match") { assert(!parse("\\S").stringMatcher(" ")) }

  test("word character match") {
    val gen = Gen.oneOf(Gen.alphaNumChar, Gen.const('_'))
    forAll(gen) { s =>
      assert(parse("\\w").stringMatcher(s.toString))
    }
  }

  test("whitespace character match") {
    val gen = Gen.oneOf('\t', '\n', '\f', '\r', ' ')
    forAll(gen) { s =>
      assert(parse("\\s").stringMatcher(s.toString))
    }
  }

  test("repeat examples") {
    val r = lit('b').repeat(2, Some(4))
    val m = r.stringMatcher
    m("") should ===(false)
    m("b") should ===(false)
    m("bb") should ===(true)
    m("bbb") should ===(true)
    m("bbbb") should ===(true)
    m("bbbbb") should ===(false)
    m("bcb") should ===(false)
  }

  test("repeat(0, n) matches empty") {
    forAll(arbitrary[Regex[Int]], Gen.option(Gen.chooseNum(0, 20))) { (r, max) =>
      assert(r.repeat(0, max).matcher[List].apply(List.empty))
    }
  }

  test("repeat(0, 0) doesn't match non-empty") {
    forAll(arbitrary[Regex[Int]], Gen.nonEmptyListOf(arbitrary[Int])) { (r, c) =>
      assert(!r.repeat(0, Some(0)).matcher[List].apply(c))
    }
  }

  test("general regex matching") {
    forAll(genIntRegexAndMatch) { rm =>
      assert(rm.r.matcher[Stream].apply(rm.candidate))
    }
  }

  test("or(r, r) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(rc.r, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(impossible, r) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(impossible, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(r, impossible) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = or(rc.r, impossible)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("andThen(empty, r) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = andThen(Regex.empty, rc.r)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("andThen(r, empty) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int]) =>
      val expected = rc.r.matcher[Stream].apply(rc.candidate)
      val equivR = andThen(rc.r, Regex.empty)
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("if r matches, oneOrMore(r) matches") {
    forAll(genIntRegexAndMatch) { rc =>
      assert(oneOrMore(rc.r).matcher[Stream].apply(rc.candidate))
    }
  }

  test("if r matches x, oneOrMore(r) matches n * x") {
    forAll(genIntRegexAndMatch, Gen.chooseNum(1, 10)) { (rc, n) =>
      oneOrMore(rc.r).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("if r matches x, star(r) matches n * x") {
    forAll(genIntRegexAndMatch, Gen.chooseNum(0, 10)) { (rc, n) =>
      star(rc.r).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("repeat(n, n, r) is equivalent to count(n, r)") {
    forAll(arbitrary[RegexAndCandidate[Int]], Gen.chooseNum(1, 10)) { (rc, n) =>
      val expected = rc.r.count(n).matcher[Stream].apply(rc.candidate)
      val equivR = rc.r.repeat(n, Some(n))
      val actual = equivR.matcher[Stream].apply(rc.candidate)
      actual should ===(expected)
    }
  }

  test("repeat(n, m, r) matches r.count(n) * r.star") {
    val gen = for {
      min <- Gen.chooseNum(0, 10)
      plus <- Gen.chooseNum(0, 5)
      r <- arbitrary[Regex[Int]]
      rRepeat = r.repeat(min, Some(min + plus))
      c <- regexMatchingStreamGen(intMatchingGen).apply(rRepeat)
    } yield (min, r, c)

    forAll(gen) {
      case (min, r, c) =>
        val r2 = r.count(min) * r.star
        assert(r2.matcher[Stream].apply(c))
    }
  }

  test("oneOfF consistent with oneOf") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      r1 = oneOfF(values)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = Regex.oneOf(values.head, values.tail: _*)
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("oneOfF consistent with oneOfFR") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      r1 = oneOfF(values)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = oneOfFR(values.map(lit(_)))
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("oneOfR consistent with oneOfFR") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      lits = values.map(lit(_))
      r1 = oneOfFR(lits)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (lits, r1, c)
    forAll(gen) {
      case (lits, r1, c) =>
        val r2 = oneOfR(lits.head, lits.tail: _*)
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("seq consistent with allOf") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      r1 = seq(values)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = Regex.allOf(values: _*)
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("allOfF consistent with allOf") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      r1 = allOfF(values)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = Regex.allOf(values: _*)
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("allOfF consistent with allOfFR") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      r1 = allOfF(values)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = allOfFR(values.map(lit(_)))
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }

  test("allOfR consistent with allOfFR") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      lits = values.map(lit(_))
      r1 = allOfR(lits: _*)
      c <- genCandidateStream(byteMatchingGen)(r1)
    } yield (lits, r1, c)
    forAll(gen) {
      case (lits, r1, c) =>
        val r2 = allOfFR(lits)
        r1.matcher[Stream].apply(c) should ===(r2.matcher[Stream].apply(c))
    }
  }
}
