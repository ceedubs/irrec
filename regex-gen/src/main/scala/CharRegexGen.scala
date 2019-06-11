package ceedubs.irrec
package regex

import RegexGen.{genRegex, genRangeMatch}
import RegexAndCandidate.{genRegexAndCandidate, genRegexAndMatch}

import cats.implicits._
import org.scalacheck.Gen


/**
 * This providex support for generation of `Char` regular expressions.
 *
 * Parts of Unicode are a bit weird and probably don't make much sense inside of regular
 * expressions. So for now at least we avoid the most fiddly of bits.
 */
object CharRegexGen {

  val supportedCharRangesInclusive: List[(Char, Char)] = List(
    (0x0000, 0xD7FF),
    (0xF900, 0xFFFD))

  /**
   * Adapted from code in Scalacheck.
   */
  private val genSupportedCharRangeBounds: Gen[(Char, Char)] =
    Gen.frequency((supportedCharRangesInclusive.map {
      case (first, last) => (last + 1 - first, Gen.const((first, last)))
    }: List[(Int, Gen[(Char, Char)])]): _*)

  val genSupportedChar: Gen[Char] =
    for {
      bounds <- genSupportedCharRangeBounds
      (min, max) = bounds
      c <- Gen.choose(min, max)
    } yield c

  val genSupportedCharMatchRange: Gen[Match.Range[Char]] =
    for {
      bounds <- genSupportedCharRangeBounds
      (min, max) = bounds
      lower <- Gen.choose(min, max)
      upper <- Gen.choose(lower, max)
    } yield Match.Range(lower, upper)

  def genRegexChar(includeZero: Boolean, includeOne: Boolean): Gen[Regex[Char]] = genRegex(genSupportedChar, genSupportedCharMatchRange, includeZero = includeZero, includeOne = includeOne)

  val genStandardRegexChar: Gen[Regex[Char]] = genRegexChar(includeZero = false, includeOne = false)

  val genAlphaNumCharRegex: Gen[Regex[Char]] = genRegex(Gen.alphaNumChar, genRangeMatch(Gen.alphaNumChar), includeZero = false, includeOne = false)

  val genCharRegexAndMatch: Gen[RegexAndCandidate[Char]] = genRegexAndMatch(includeOne = false, genSupportedChar, genSupportedCharMatchRange)

  val genAlphaNumCharRegexAndMatch: Gen[RegexAndCandidate[Char]] = genRegexAndMatch(includeOne = false, Gen.alphaNumChar, genRangeMatch(Gen.alphaNumChar))

  val genCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] = genRegexAndCandidate(genSupportedChar, genSupportedCharMatchRange, includeZero = false, includeOne = false)

  val genAlphaNumCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] = genRegexAndCandidate(Gen.alphaNumChar, genRangeMatch(Gen.alphaNumChar), includeZero = false, includeOne = false)
}
