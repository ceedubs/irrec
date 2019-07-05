package ceedubs.irrec
package regex

import DietGen.dietMatchingGen

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

  val supportedCharacters: Diet[Char] =
    Diet.fromRange(Range('\u0000', '\uD7FF')).addRange(Range('\uF900', '\uFFFD'))

  def regexMatchingStringGenFromDiet(available: Diet[Char]): Regex[Char] => Gen[String] = {
    val streamGen = RegexMatchGen.dietRegexMatchingStreamGen(available)
    r => streamGen(r).map(_.mkString)
  }

  val regexMatchingStringGen: Regex[Char] => Gen[String] =
    regexMatchingStringGenFromDiet(supportedCharacters)

  val standardCharRegexGenConfig: RegexGen.Config[Char] =
    RegexGen.Config
      .fromDiscreteDiet(supportedCharacters)

  val genStandardRegexChar: Gen[Regex[Char]] = RegexGen.genRegex(standardCharRegexGenConfig)

  val genAlphaNumCharRegex: Gen[Regex[Char]] =
    RegexGen.genRegex(RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric))

  val genAlphaNumCharRegexAndMatch: Gen[RegexAndCandidate[Char]] =
    RegexAndCandidate.genRegexAndMatch(
      RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric),
      RegexMatchGen.dietMatchToGen[Char](CharacterClasses.alphaNumeric, dietMatchingGen(_)))

  val genAlphaNumCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] =
    RegexAndCandidate.genRegexAndCandidate(
      RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric),
      RegexMatchGen.dietMatchToGen[Char](CharacterClasses.alphaNumeric, dietMatchingGen(_)))

  val genCharRegexAndMatch: Gen[RegexAndCandidate[Char]] =
    RegexAndCandidate.genRegexAndMatch(
      standardCharRegexGenConfig,
      RegexMatchGen.dietMatchToGen[Char](supportedCharacters, dietMatchingGen(_)))

  val genCharRegexAndCandidate: Gen[RegexAndCandidate[Char]] =
    RegexAndCandidate.genRegexAndCandidate(
      RegexGen.Config.fromDiscreteDiet(supportedCharacters),
      RegexMatchGen.dietMatchToGen(supportedCharacters, dietMatchingGen(_)))

  implicit val arbCharRegex: Arbitrary[Regex[Char]] = Arbitrary(CharRegexGen.genStandardRegexChar)
}
