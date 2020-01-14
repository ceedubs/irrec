package ceedubs.irrec
package regex
package applicative

import Regex._
import char._
import RegexGen._
import ceedubs.irrec.parse.{regex2 => parse}
import RegexAndCandidate.{genCandidateStream, genIntRegexAndMatch}
import RegexMatchGen.regexMatchingStreamGen
import ceedubs.irrec.regex.RegexMatchGen.{byteMatchingGen, intMatchingGen}

import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import cats.laws.discipline.arbitrary._

// TODO move some of these out to CharMatchTests or whatever?
class RegexMatchTests extends IrrecSuite {
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
      literal('b').oneOrMore(g).compile.parseOnlyS("bb") should ===(
        Some(NonEmptyChain('b', 'b')))
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
    Regex.oneOf('a', 'b', 'c').compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("oneOf second match") {
    Regex.oneOf('a', 'b', 'c').compile.parseOnlyS("b") should ===(Some('b'))
  }

  test("oneOf last match") {
    Regex.oneOf('a', 'b', 'c').compile.parseOnlyS("c") should ===(Some('c'))
  }

  test("oneOf non match") { Regex.oneOf('a', 'b', 'c').compile.parseOnlyS("d") should ===(None) }

  test("seq empty match") { seq("").compile.parseOnlyS("") should ===(Some(Chain.empty)) }

  test("seq empty non-match") { seq("").compile.parseOnlyS("a") should ===(None) }

  test("seq single match") { seq("a").compile.parseOnlyS("a") should ===(Some(Chain.one('a'))) }

  test("seq match") { seq("abc").compile.parseOnlyS("abc") should ===(Some(Chain('a', 'b', 'c'))) }

  test("seq non-match") { seq("abc").compile.parseOnlyS("bcd") should ===(None) }

  test("optional match present") {
    (lit('a') product lit('b').optional product lit('c')).compile.parseOnlyS("abc") should ===(
      Some((('a', 'b'.some), 'c')))
  }

  test("optional match not present") {
    (lit('a') product lit('b').optional product lit('c')).compile.parseOnlyS("ac") should ===(
      Some((('a', None), 'c')))
  }

  // TODO some of these might be better tests with a withMatching call
  test("character class literal match middle") {
    parse("a[bd-fhj]l").compile.parseOnlyS("ahl") should ===(Some(()))
  }

  test("character class literal match end") {
    parse("a[bd-fh]j").compile.parseOnlyS("ahj") should ===(Some(()))
  }

  test("character class literal non-match") {
    parse("a[bd-fh]j").compile.parseOnlyS("axj") should ===(None)
  }

  test("character class range match beginning") {
    parse("a[d-fh]j").compile.parseOnlyS("aej") should ===(Some(()))
  }

  test("character class range match end") {
    parse("a[bd-f]j").compile.parseOnlyS("aej") should ===(Some(()))
  }

  test("character class range non-match") {
    parse("a[d-fh]j").compile.parseOnlyS("axj") should ===(None)
  }

  test("character class range match low") {
    parse("a[bd-fh]j").compile.parseOnlyS("adj") should ===(Some(()))
  }

  test("character class range match high") {
    parse("a[bd-fh]j").compile.parseOnlyS("afj") should ===(Some(()))
  }

  test("digit character single match") { digit.compile.parseOnlyS("2") should ===(Some(2)) }

  test("digit character non-match") { digit.compile.parseOnlyS("a") should ===(None) }

  test("non-digit character single match") {
    nonDigit.compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("non-digit character non-match") { nonDigit.compile.parseOnlyS("3") should ===(None) }

  test("word character single match") { wordChar.compile.parseOnlyS("a") should ===(Some('a')) }

  test("word character non-match") { wordChar.compile.parseOnlyS("%") should ===(None) }

  test("non-word character single match") {
    nonWordChar.compile.parseOnlyS("%") should ===(Some('%'))
  }

  test("non-word character non-match") { nonWordChar.compile.parseOnlyS("a") should ===(None) }

  test("whitespace character single match") {
    whitespaceChar.compile.parseOnlyS(" ") should ===(Some(' '))
  }

  test("whitespace character non-match") { whitespaceChar.compile.parseOnlyS("%") should ===(None) }

  test("whitespace character negated range match") {
    lit('a').product(nonWhitespaceChar).product(lit('c')).compile.parseOnlyS("abc") should ===(
      Some((('a', 'b'), 'c')))
  }

  test("whitespace character negated range non-match") {
    lit('a').product(nonWhitespaceChar).product(lit('c')).compile.parseOnlyS("a c") should ===(None)
  }

  test("non-whitespace character single match") {
    nonWhitespaceChar.compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("non-whitespace character non-match") {
    nonWhitespaceChar.compile.parseOnlyS(" ") should ===(None)
  }

  test("word character match") {
    val gen = Gen.oneOf(Gen.alphaNumChar, Gen.const('_'))
    // TODO needing to specify Char here is annoying. Look into how to make this not so painful
    val rc = wordChar.compile[Char]
    forAll(gen) { c =>
      rc.parseOnlyS(c.toString) should ===(Some(c))
    }
  }

  test("whitespace character match") {
    val gen = Gen.oneOf('\t', '\n', '\f', '\r', ' ')
    val rc = whitespaceChar.compile[Char]
    forAll(gen) { c =>
      rc.parseOnlyS(c.toString) should ===(Some(c))
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
    forAll(arbitrary[Regex[Int, Unit]], Gen.option(Gen.chooseNum(0, 20)), arbitrary[Greediness]) { (r, max, g) =>
      r.repeat(0, max, g).void.compile.parseOnly(List.empty) should ===(Some(()))
    }
  }

  test("repeat(0, 0) doesn't match non-empty") {
    forAll(arbitrary[Regex[Int, Unit]], Gen.nonEmptyListOf(arbitrary[Int]), arbitrary[Greediness]) { (r, c, g) =>
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
      val equivR = or(Regex.fail, rc.r)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("or(r, impossible) is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = or(rc.r, Regex.fail)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("empty *> r is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = Regex.empty[Int, Match[Int]] *> rc.r
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("r <* empty is equivalent to r") {
    forAll { (rc: RegexAndCandidate[Int, Long]) =>
      val expected = rc.r.compile.parseOnly(rc.candidate)
      val equivR = rc.r <* Regex.empty
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
      rc.r.oneOrMore(g).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(
        true)
    }
  }

  test("if r matches x, r.star matches n * x") {
    forAll(genIntRegexAndMatch[Long], Gen.chooseNum(0, 10), arbitrary[Greediness]) { (rc, n, g) =>
      rc.r.star(g).matcher[Stream].apply(Stream.fill(n)(rc.candidate).flatten) should ===(true)
    }
  }

  test("repeat(n, n, r) is equivalent to count(n, r)") {
    forAll(arbitrary[RegexAndCandidate[Int, Long]], Gen.chooseNum(1, 10), genGreediness) { (rc, n, g) =>
      val expected = rc.r.count(n).compile.parseOnly(rc.candidate)
      val equivR = rc.r.repeat(n, Some(n), g)
      val actual = equivR.compile.parseOnly(rc.candidate)
      actual should ===(expected)
    }
  }

  test("r.repeat(m, n, g) consistent with r.count(n) then r.star") {
    val gen = for {
      min <- Gen.chooseNum(0, 10)
      plus <- Gen.chooseNum(0, 5)
      r <- arbitrary[Regex[Int, Long]]
      g <- arbitrary[Greediness]
      rRepeat = r.repeat(min, Some(min + plus), g)
      c <- regexMatchingStreamGen(intMatchingGen).apply(rRepeat)
      g <- arbitrary[Greediness]
    } yield (min, r, rRepeat, g, c)

    forAll(gen) {
      case (min, r, rRepeat, g, c) =>
        val rCount = r.count(min).map2(r.star(g))(_ ++ _)
        rCount.compile.parseOnly(c) should ===(rRepeat.compile.parseOnly(c))
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
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
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
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
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
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
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
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
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
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
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
        val r2 = allOfFR(Chain.fromSeq(lits))
        r1.compile.parseOnly(c) should ===(r2.compile.parseOnly(c))
    }
  }
}
