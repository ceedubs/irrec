package ceedubs.irrec
package regex

import ceedubs.irrec.regex.combinator._
import ceedubs.irrec.regex.{combinator => C}
import Greediness._
import gen.{CharRegexGen, RegexAndCandidate}
import gen.RegexGen._
import ceedubs.irrec.parse.{regex => parse}
import gen.RegexAndCandidate.genIntRegexAndMatch
import gen.RegexMatchGen.genRegexMatch

import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import cats.implicits._
import cats.laws.discipline.arbitrary._

class CombinatorMatchTests extends IrrecSuite {
  test("elem match") {
    elem((x: Int) => if (x % 2 == 0) Some(x) else None).compile.parseOnly(List(4)) should ===(
      Some(4))
  }

  test("elem non-match") {
    elem((x: Int) => if (x % 2 == 0) Some(x) else None).compile.parseOnly(List(3)) should ===(None)
  }

  test("pred match") { pred((_: Int) % 2 == 0).compile.parseOnly(List(4)) should ===(Some(4)) }

  test("pred non-match") { pred((_: Int) % 2 == 0).compile.parseOnly(List(3)) should ===(None) }

  test("literal match") { literal('b').compile.parseOnlyS("b") should ===(Some('b')) }

  test("literal non-match") { literal('b').compile.parseOnlyS("a") should ===(None) }

  test("literal with trailing") { literal('b').compile.parseOnlyS("ba") should ===(None) }

  test("or left match") {
    or(literal('b'), literal('c')).compile.parseOnlyS("b") should ===(Some('b'))
  }

  test("or left match with trailing") {
    or(literal('b'), literal('c')).compile.parseOnlyS("bc") should ===(None)
  }

  test("or right match") {
    or(literal('b'), literal('c')).compile.parseOnlyS("c") should ===(Some('c'))
  }

  test("or right match with trailing") {
    or(literal('b'), literal('c')).compile.parseOnlyS("cb") should ===(None)
  }

  test("or no match") { or(literal('b'), literal('c')).compile.parseOnlyS("a") should ===(None) }

  test("or no match with trailing") {
    or(literal('b'), literal('c')).compile.parseOnlyS("ad") should ===(None)
  }

  test("either left match") {
    either(literal('b'), seq("bc")).compile.parseOnlyS("b") should ===(Some(Left('b')))
  }

  test("either left match with trailing") {
    either(literal('b'), seq("bc")).compile.parseOnlyS("bjk") should ===(None)
  }

  test("either right match") {
    either(literal('b'), seq("bc")).compile.parseOnlyS("bc") should ===(
      Some(Right(Chain('b', 'c'))))
  }

  test("either right match with trailing") {
    either(literal('b'), seq("bc")).compile.parseOnlyS("bce") should ===(None)
  }

  test("either no match") {
    either(literal('b'), seq("bc")).compile.parseOnlyS("a") should ===(None)
  }

  test("either no match with trailing") {
    literal('b').either(seq("bc")).compile.parseOnlyS("ade") should ===(None)
  }

  test("product match") {
    literal('b').product(literal('c')).compile.parseOnlyS("bc") should ===(Some(('b', 'c')))
  }

  test("product left only") {
    literal('b').product(literal('c')).compile.parseOnlyS("bd") should ===(None)
  }

  test("productkh right only") {
    literal('b').product(literal('c')).compile.parseOnlyS("ac") should ===(None)
  }

  test("product with trailing") {
    literal('b').product(literal('c')).compile.parseOnlyS("bcd") should ===(None)
  }

  test("many zero") { literal('b').many.compile.parseOnlyS("") should ===(Some(Chain.empty)) }

  test("many one") { literal('b').many.compile.parseOnlyS("b") should ===(Some(Chain.one('b'))) }

  test("many two") { literal('b').many.compile.parseOnlyS("bb") should ===(Some(Chain('b', 'b'))) }

  test("many three") {
    or(literal('b'), literal('c')).many.compile.parseOnlyS("bcb") should ===(
      Some(Chain('b', 'c', 'b')))
  }

  test("many trailing") {
    or(literal('b'), literal('c')).many.compile.parseOnlyS("bcbd") should ===(None)
  }

  test("few zero") { literal('b').few.compile.parseOnlyS("") should ===(Some(Chain.empty)) }

  test("few one") { literal('b').few.compile.parseOnlyS("b") should ===(Some(Chain.one('b'))) }

  test("few two") { literal('b').few.compile.parseOnlyS("bb") should ===(Some(Chain('b', 'b'))) }

  test("few three") {
    or(literal('b'), literal('c')).few.compile.parseOnlyS("bcb") should ===(
      Some(Chain('b', 'c', 'b')))
  }

  test("few trailing") {
    or(literal('b'), literal('c')).few.compile.parseOnlyS("bcbd") should ===(None)
  }

  test("wildcard") { wildcard[Char].compile.parseOnlyS("b") should ===(Some('b')) }

  test("wildcard trailing") { wildcard[Char].compile.parseOnlyS("bc") should ===(None) }

  test("wildcard empty") { wildcard[Char].compile.parseOnlyS("") should ===(None) }

  test("inside range") { range('a', 'c').compile.parseOnlyS("b") should ===(Some('b')) }

  test("left range") { range('a', 'c').compile.parseOnlyS("a") should ===(Some('a')) }

  test("right range") { range('a', 'c').compile.parseOnlyS("c") should ===(Some('c')) }

  test("outside range") { range('a', 'c').compile.parseOnlyS("d") should ===(None) }

  test("oneOrMore zero") {
    forAll { (g: Greediness) =>
      literal('b').oneOrMore(g).compile.parseOnlyS("") should ===(None)
    }
  }

  test("oneOrMore one") {
    forAll { (g: Greediness) =>
      literal('b').oneOrMore(g).compile.parseOnlyS("b") should ===(Some(NonEmptyChain('b')))
    }
  }

  test("oneOrMore two") {
    forAll { (g: Greediness) =>
      literal('b').oneOrMore(g).compile.parseOnlyS("bb") should ===(Some(NonEmptyChain('b', 'b')))
    }
  }

  test("oneOrMore three") {
    forAll { (g: Greediness) =>
      literal('b').oneOrMore(g).compile.parseOnlyS("bbb") should ===(
        Some(NonEmptyChain('b', 'b', 'b')))
    }
  }

  test("count zero empty") {
    literal('b').count(0).compile.parseOnlyS("") should ===(Some(Chain.empty))
  }

  test("count zero non-empty") { literal('b').count(0).compile.parseOnlyS("b") should ===(None) }

  test("count 1 empty") { literal('b').count(1).compile.parseOnlyS("") should ===(None) }

  test("count 1 match") {
    literal('b').count(1).compile.parseOnlyS("b") should ===(Some(Chain.one('b')))
  }

  test("count 1 non-match") { literal('b').count(1).compile.parseOnlyS("c") should ===(None) }

  test("count 2 match") {
    literal('b').count(2).compile.parseOnlyS("bb") should ===(Some(Chain('b', 'b')))
  }

  test("count 2 non-match") { literal('b').count(2).compile.parseOnlyS("bc") should ===(None) }

  test("oneOf first match") {
    C.oneOf('a', 'b', 'c').compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("oneOf second match") {
    C.oneOf('a', 'b', 'c').compile.parseOnlyS("b") should ===(Some('b'))
  }

  test("oneOf last match") {
    C.oneOf('a', 'b', 'c').compile.parseOnlyS("c") should ===(Some('c'))
  }

  test("oneOf non match") { C.oneOf('a', 'b', 'c').compile.parseOnlyS("d") should ===(None) }

  test("seq empty match") { seq("").compile.parseOnlyS("") should ===(Some(Chain.empty)) }

  test("seq empty non-match") { seq("").compile.parseOnlyS("a") should ===(None) }

  test("seq single match") { seq("a").compile.parseOnlyS("a") should ===(Some(Chain.one('a'))) }

  test("seq match") { seq("abc").compile.parseOnlyS("abc") should ===(Some(Chain('a', 'b', 'c'))) }

  test("seq non-match") { seq("abc").compile.parseOnlyS("bcd") should ===(None) }

  test("optional match present") {
    forAll { g: Greediness =>
      (lit('a') product lit('b').optional(g) product lit('c')).compile.parseOnlyS("abc") should ===(
        Some((('a', 'b'.some), 'c')))
    }
  }

  test("optional_ match present") {
    (lit('a') <* lit('b').optional_ product lit('c')).compile.parseOnlyS("abc") should ===(
      Some(('a', 'c')))
  }

  test("non-greedy optional match") {
    (lit('a') product wildcard[Char].optional(NonGreedy) product lit('b')).compile
      .parseOnlyS("ab") should ===(Some((('a', None), 'b')))
  }

  test("optional match not present") {
    forAll { g: Greediness =>
      (lit('a') product lit('b').optional(g) product lit('c')).compile.parseOnlyS("ac") should ===(
        Some((('a', None), 'c')))
    }
  }

  test("optional_ match not present") {
    (lit('a') <* lit('b').optional_ product lit('c')).compile.parseOnlyS("ac") should ===(
      Some(('a', 'c')))
  }

  test("star_ match 0") {
    (lit('a').star_ *> lit('b')).compile.parseOnlyS("b") should ===(Some('b'))
  }

  test("star_ match 1") {
    (lit('a').star_ *> lit('b')).compile.parseOnlyS("ab") should ===(Some('b'))
  }

  test("star_ match multiple") {
    (lit('a').star_ *> lit('b')).compile.parseOnlyS("aab") should ===(Some('b'))
  }

  test("chain consistent with starFold") {
    val gen = for {
      r0 <- arbitrary[RegexC[Int]]
      g <- arbitrary[Greediness]
      rStar = r0.starFold(g, Chain.empty[Int])(_ append _)
      candidate <- Gen.oneOf(CharRegexGen.genRegexMatchingString(rStar), arbitrary[String])
    } yield (r0, g, rStar, candidate)
    forAll(gen) {
      case (r0, g, rStar, candidate) =>
        val rChain = r0.star(g)

        rChain.compile.parseOnlyS(candidate) should ===(rStar.compile.parseOnlyS(candidate))
    }
  }

  test("greedy chain") {
    forAll { (g: Greediness) =>
      val r = wildcard[Char].star(Greedy).product(lit('a').oneOrMore(g))
      r.compile.parseOnlyS("aaaaa") should ===(
        Some((Chain('a', 'a', 'a', 'a'), NonEmptyChain('a'))))
    }
  }

  test("non-greedy chain") {
    forAll { (g: Greediness) =>
      val r = wildcard[Char].star(NonGreedy).product(lit('a').oneOrMore(g))
      r.compile.parseOnlyS("aaaaa") should ===(
        Some((Chain.empty, NonEmptyChain('a', 'a', 'a', 'a', 'a'))))
    }
  }

  test("greedy repeat") {
    forAll { (g: Greediness) =>
      val r = wildcard[Char].repeat(1, Some(6), Greedy).product(lit('a').oneOrMore(g))
      r.compile.parseOnlyS("aaaaa") should ===(
        Some((Chain('a', 'a', 'a', 'a'), NonEmptyChain('a'))))
    }
  }

  test("non-greedy repeat") {
    forAll { (g: Greediness) =>
      val r = wildcard[Char].repeat(1, Some(6), NonGreedy).product(lit('a').oneOrMore(g))
      r.compile.parseOnlyS("aaaaa") should ===(
        Some((Chain('a'), NonEmptyChain('a', 'a', 'a', 'a'))))
    }
  }

  test("withMatched") {
    val r = lit('a') *> parse("(b|c|d)e").void.withMatched <* lit('f')
    r.compile.parseOnlyS("acef") should ===(Some((Chain('c', 'e'), ())))
  }

  test("withMatched captures entire input for parseOnly(s)") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val actual = rc.r.withMatched.compile.parseOnly(rc.candidate)
      val expected = rc.r.compile.parseOnly(rc.candidate).map(l => (Chain.fromSeq(rc.candidate), l))
      actual should ===(expected)
    }
  }

  test("matched") {
    val r = lit('a') *> parse("(b|c|d)e").matched <* lit('f')
    r.compile.parseOnlyS("acef") should ===(Some(Chain('c', 'e')))
  }

  test("matched captures entire input for parseOnly(s)") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val actual = rc.r.matched.compile.parseOnly(rc.candidate)
      val expected =
        if (rc.r.matcher[Stream].apply(rc.candidate)) Some(Chain.fromSeq(rc.candidate)) else None
      actual should ===(expected)
    }
  }

  test("repeat examples") {
    forAll { g: Greediness =>
      val m: String => Option[Chain[Char]] = lit('b').repeat(2, Some(4), g).compile.parseOnlyS(_)
      m("") should ===(None)
      m("b") should ===(None)
      m("bb") should ===(Some(Chain('b', 'b')))
      m("bbb") should ===(Some(Chain('b', 'b', 'b')))
      m("bbbb") should ===(Some(Chain('b', 'b', 'b', 'b')))
      m("bbbbb") should ===(None)
      m("bcb") should ===(None)
    }
  }

  test("repeat(0, n) matches empty") {
    forAll(arbitrary[RegexM[Int, Unit]], Gen.option(Gen.chooseNum(0, 20)), arbitrary[Greediness]) {
      (r, max, g) =>
        r.repeat(0, max, g).void.compile.parseOnly(List.empty[Int]) should ===(Some(()))
    }
  }

  test("repeat(0, 0) doesn't match non-empty") {
    forAll(arbitrary[RegexM[Int, Unit]], Gen.nonEmptyListOf(arbitrary[Int]), arbitrary[Greediness]) {
      (r, c, g) =>
        r.repeat(0, Some(0), g).void.compile.parseOnly(c) should ===(None)
    }
  }

  test("general regex matching") {
    forAll(genIntRegexAndMatch[Unit]) { rm =>
      rm.r.compile.parseOnly(rm.candidate) should ===(Some(()))
    }
  }

  test("or(r, r) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = or(rc.r, rc.r)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(impossible, r) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = or(C.fail, rc.r)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(r, impossible) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = or(rc.r, C.fail)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("empty *> r is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = C.empty[Int, Match[Int]] *> rc.r
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("r <* empty is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = rc.r <* C.empty
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("if r matches, r.oneOrMore matches") {
    forAll(genIntRegexAndMatch[Long], arbitrary[Greediness]) { (rc, g) =>
      assert(rc.r.oneOrMore(g).matcher[Stream].apply(rc.candidate))
    }
  }

  test("if r matches x, r.oneOrMore matches n * x") {
    forAll(genIntRegexAndMatch[Long], Gen.chooseNum(1, 10), arbitrary[Greediness]) { (rc, n, g) =>
      rc.r.oneOrMore(g).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("if r matches x, r.chain matches n * x") {
    forAll(genIntRegexAndMatch[Long], Gen.chooseNum(0, 10), arbitrary[Greediness]) { (rc, n, g) =>
      rc.r.star(g).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("repeat(n, n, r) is equivalent to count(n, r)") {
    forAll(arbitrary[RegexAndCandidate[Int, Long]], Gen.chooseNum(1, 10), genGreediness) {
      (rc, n, g) =>
        val expected = rc.r.count(n).compile.parseOnly(rc.candidate)
        val equivR = rc.r.repeat(n, Some(n), g)
        val actual = equivR.compile.parseOnly(rc.candidate)
        actual should ===(expected)
    }
  }

  test("r.repeat(m, n, g) consistent with r.count(n) then r.chain") {
    val gen = for {
      min <- Gen.chooseNum(0, 10)
      plus <- Gen.chooseNum(0, 5)
      r <- arbitrary[RegexM[Int, Long]]
      g <- arbitrary[Greediness]
      rRepeat = r.repeat(min, Some(min + plus), g)
      c <- genRegexMatch(rRepeat)
      g <- arbitrary[Greediness]
    } yield (min, r, rRepeat, g, c)

    forAll(gen) {
      case (min, r, rRepeat, g, c) =>
        val rCount = r.count(min).map2(r.star(g))(_ ++ _)
        rCount.matcher[Stream].apply(c) should ===(rRepeat.matcher[Stream].apply(c))
    }
  }

  test("oneOfF consistent with oneOf") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      r1 = oneOfF(values)
      c <- genRegexMatch(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = C.oneOf(values.head, values.tail: _*)
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }

  test("oneOfF consistent with oneOfFR") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      r1 = oneOfF(values)
      c <- genRegexMatch(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = oneOfFR(values.map(lit(_)))
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }

  test("oneOfR consistent with oneOfFR") {
    val gen = for {
      values <- arbitrary[NonEmptyList[Byte]]
      lits = values.map(lit(_))
      r1 = oneOfFR(lits)
      c <- genRegexMatch(r1)
    } yield (lits, r1, c)
    forAll(gen) {
      case (lits, r1, c) =>
        val r2 = oneOfR(lits.head, lits.tail: _*)
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }

  test("seq consistent with allOf") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      r1 = seq(values)
      c <- genRegexMatch(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = C.allOf(values: _*)
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }

  test("allOfF consistent with allOfFR") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      r1 = allOfF(values)
      c <- genRegexMatch(r1)
    } yield (values, r1, c)
    forAll(gen) {
      case (values, r1, c) =>
        val r2 = allOfFR(values.map(lit(_)))
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }

  test("allOfR consistent with allOfFR") {
    val gen = for {
      values <- arbitrary[List[Byte]]
      lits = values.map(lit(_))
      r1 = allOfR(lits: _*)
      c <- genRegexMatch(r1)
    } yield (lits, r1, c)
    forAll(gen) {
      case (lits, r1, c) =>
        val r2 = allOfFR(Chain.fromSeq(lits))
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }
}
