package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Regex._
import ceedubs.irrec.regex.RegexGen._
import org.scalacheck.Gen, Gen.Choose
import org.scalacheck.Arbitrary, Arbitrary.arbitrary

class GlushkovTests extends IrrecSuite {
  import GlushkovTests._

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

  test("general regex matching"){
    forAll(genRegexAndMatch[Int]) { rm =>
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
}

object GlushkovTests {
  final case class RegexAndCandidate[A](r: Regex[A], candidate: Stream[A])

  def genRegexAndMatch[A](implicit arbA: Arbitrary[A], chooseA: Choose[A], orderingA: Ordering[A]): Gen[RegexAndCandidate[A]] =
    for {
      r <- genRegex(arbitrary[A], false)
      c <- r(regexMatchingStreamGen(arbitrary[A]))
    } yield RegexAndCandidate(r, c)

  /**
   * Generates arbitrary regexes and candidate matches for the regex. The candidate will match the
   * regex roughly 50% of the time.
   */
  implicit def arbRegexAndCandidate[A](implicit arbA: Arbitrary[A], chooseA: Choose[A], orderingA: Ordering[A]): Arbitrary[RegexAndCandidate[A]] = {
    val probablyNotMatching = for {
      r <- genRegex(arbitrary[A], true)
      c <- arbitrary[Stream[A]]
    } yield RegexAndCandidate(r, c)

    Arbitrary(Gen.oneOf(probablyNotMatching, genRegexAndMatch[A]))
  }
}
