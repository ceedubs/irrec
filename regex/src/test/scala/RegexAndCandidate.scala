package ceedubs.irrec
package regex

import ceedubs.irrec.regex.RegexGen._
import qq.droste.scheme
import qq.droste.data.prelude._
import org.scalacheck.Gen, Gen.Choose
import org.scalacheck.Arbitrary, Arbitrary.arbitrary

final case class RegexAndCandidate[A](r: Regex[A], candidate: Stream[A])

object RegexAndCandidate {

  def genRegexAndMatch[A](includeOne: Boolean, genA: Gen[A])(implicit chooseA: Choose[A], orderingA: Ordering[A]): Gen[RegexAndCandidate[A]] =
    for {
      r <- genRegex(genA, includeZero = false, includeOne = includeOne)
      c <- scheme.cata(regexMatchingStreamGen(genA)).apply(r)
    } yield RegexAndCandidate(r, c)

  /**
   * Generates arbitrary regexes and candidate matches for the regex. The candidate will match the
   * regex roughly 50% of the time.
   */
  implicit def arbRegexAndCandidate[A](implicit arbA: Arbitrary[A], chooseA: Choose[A], orderingA: Ordering[A]): Arbitrary[RegexAndCandidate[A]] = {
    val probablyNotMatching = for {
      r <- genRegex(arbitrary[A], includeZero = true, includeOne = true)
      c <- arbitrary[Stream[A]]
    } yield RegexAndCandidate(r, c)

    Arbitrary(Gen.oneOf(probablyNotMatching, genRegexAndMatch[A](includeOne = true, arbA.arbitrary)))
  }
}
