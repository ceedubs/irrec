package ceedubs.irrec
package regex

// TODO ceedubs
//import RegexGen.{genRangeMatch, genRegex}
//import RegexAndCandidate.{genRegexAndCandidate, genRegexAndMatch}

import cats.implicits._
import org.scalacheck.{Arbitrary, Gen}
import cats.collections.{Diet, Range}

/**
 * This providex support for generation of `Char` regular expressions.
 *
 * Parts of Unicode are a bit weird and probably don't make much sense inside of regular
 * expressions. So for now at least we avoid the most fiddly of bits.
 */
object CharRegexGen {

  val supportedCharRangesInclusive: List[(Char, Char)] = List((0x0000, 0xD7FF), (0xF900, 0xFFFD))

  // TODO ceedubs should something like this go into CharacterClasses?
  // TODO ceedubs remove explicit Char params when using a different version of cats-collections
  val supportedCharacters: Diet[Char] =
    Diet
      .empty[Char]
      .addRange(Range(0x0000, 0xD7FF))
      .addRange(Range(0xF900, 0xFFFD))

  /**
   * Adapted from code in Scalacheck.
   */
  //private val genSupportedCharRangeBounds: Gen[(Char, Char)] =
  //  Gen.frequency((supportedCharRangesInclusive.map {
  //    case (first, last) => (last + 1 - first, Gen.const((first, last)))
  //  }: List[(Int, Gen[(Char, Char)])]): _*)

  // TODO ceedubs is this needed?
  //val genSupportedChar: Gen[Char] =
  //  RegexGen.dietMatchingGen[Char](supportedCharacters, RegexGen.rangeLength(_))

  //val genSupportedCharMatchRange: Gen[Match.Range[Char]] =
  //  for {
  //    bounds <- genSupportedCharRangeBounds
  //    (min, max) = bounds
  //    lower <- Gen.choose(min, max)
  //    upper <- Gen.choose(lower, max)
  //  } yield Match.Range(lower, upper)

  def regexMatchingStringGen(available: Diet[Char]): Regex[Char] => Gen[String] = {
    val streamGen = RegexMatchGen.dietRegexMatchingStreamGen(available)
    r => streamGen(r).map(_.mkString)
  }

  // TODO ceedubs naming
  val regexMatchingStandardStringGen: Regex[Char] => Gen[String] =
    regexMatchingStringGen(supportedCharacters)

  // TODO ceedubs is this useful?
  val standardCharRegexGenConfig: RegexGen.Config[Char] =
    RegexGen.Config
      .fromIntegralDiet(supportedCharacters)

  // TODO ceedubs do I have unnecssary RegeGen. prefixes in this file?
  //def genRegexChar(includeZero: Boolean, includeOne: Boolean): Gen[Regex[Char]] =
  //  RegexGen.genRegex()

  // TODO ceedubs write helper methods that make some of these irrelephant
  val genStandardRegexChar: Gen[Regex[Char]] = RegexGen.genRegex(standardCharRegexGenConfig)

  // TODO ceedubs is there a better way?
  val genAlphaNumCharRegex: Gen[Regex[Char]] =
    RegexGen.genRegex(RegexGen.Config.fromIntegralDiet(CharacterClasses.alphaNumeric))

  // TODO ceedubs should any of this be extracted?
  val genCharRegexAndMatch: Gen[RegexAndCandidate[Char]] =
    RegexAndCandidate.genRegexAndMatch(
      standardCharRegexGenConfig,
      RegexMatchGen.dietMatchToGen[Char](supportedCharacters, RegexMatchGen.dietMatchingGen(_)))

  // TODO ceedubs
  //val genAlphaNumCharRegexAndMatch: Gen[RegexAndCandidate[Char]] =
  //  genRegexAndMatch(includeOne = false, Gen.alphaNumChar, genRangeMatch(Gen.alphaNumChar))

  // TODO ceedubs can we generate a config for both things at once?
  // TODO ceedubs
  //val genCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] = RegexAndCandidate.genRegexAndCandidate(
  //  standardCharRegexGenConfig, RegexMatchGen.dietMatchingGen(supportedCharacters, RegexMatchGen.integralDietMatchingGen(_)))
  //  TODO ceedubs make it so this isn't just ASCII
  val genCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] = RegexAndCandidate.genRegexAndCandidate(
    RegexGen.Config.fromIntegralDiet(CharacterClasses.ascii), RegexMatchGen.dietMatchToGen(CharacterClasses.ascii, RegexMatchGen.dietMatchingGen(_)))

  // TODO ceedubs
  //val genAlphaNumCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] = genRegexAndCandidate(
  //  Gen.alphaNumChar,
  //  genRangeMatch(Gen.alphaNumChar),
  //  includeZero = false,
  //  includeOne = false)

  implicit val arbCharRegex: Arbitrary[Regex[Char]] = Arbitrary(CharRegexGen.genStandardRegexChar)
}
