package ceedubs.irrec
package regex

// TODO
import Regex.{Regex, RegexC}
import ceedubs.irrec.regex.DietGen.dietMatchingGen
import ceedubs.irrec.regex.RegexMatchGen.dietMatchToGen

import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.implicits._
import cats.collections.{Diet, Range}

object CharRegexGen {
  val supportedCharacters: Diet[Char] =
    Diet.fromRange(Range('\u0000', '\uD7FF')).addRange(Range('\uF900', '\uFFFD'))

  val supportedCharRegexGenConfig: RegexGen.Config[Char] =
    RegexGen.Config
      .fromDiscreteDiet(supportedCharacters)

  def genSupportedCharRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(supportedCharRegexGenConfig)

  def genAlphaNumRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric))

  def genAsciiRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(RegexGen.Config.fromDiscreteDiet(CharacterClasses.ascii))

  def genStandardCharRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] = Gen.frequency(
    5 -> genAsciiRegex[Out],
    4 -> genAlphaNumRegex[Out],
    1 -> genSupportedCharRegex[Out]
  )

  def genSupportedRegexAndMatch[Out: Arbitrary: Cogen]: Gen[RegexAndCandidate[Char, Out]] =
    RegexAndCandidate.genRegexAndMatch(
      RegexGen.Config.fromDiscreteDiet(supportedCharacters),
      dietMatchToGen[Char](supportedCharacters, dietMatchingGen(_)))

  def genAlphaNumRegexAndMatch[Out: Arbitrary: Cogen]: Gen[RegexAndCandidate[Char, Out]] =
    RegexAndCandidate.genRegexAndMatch(
      RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric),
      dietMatchToGen[Char](CharacterClasses.alphaNumeric, dietMatchingGen(_)))

  def genAlphaNumRegexAndCandidate[Out: Arbitrary: Cogen]: Gen[RegexAndCandidate[Char, Out]] =
    RegexAndCandidate.genRegexAndCandidate(
      RegexGen.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric),
      dietMatchToGen[Char](CharacterClasses.alphaNumeric, dietMatchingGen(_)))

  def genRegexAndCandidate[Out: Arbitrary: Cogen]: Gen[RegexAndCandidate[Char, Out]] =
    RegexAndCandidate.genRegexAndCandidate(
      RegexGen.Config.fromDiscreteDiet(supportedCharacters),
      dietMatchToGen(supportedCharacters, dietMatchingGen(_)))

  def regexMatchingStringGenFromDiet[Out](available: Diet[Char]): RegexC[Out] => Gen[String] = {
    val streamGen = RegexMatchGen.dietRegexMatchingStreamGen[Char, Out](available)
    r => streamGen(r).map(_.mkString)
  }

  def regexMatchingStringGen[Out]: RegexC[Out] => Gen[String] =
    regexMatchingStringGenFromDiet(supportedCharacters)

  implicit def arbCharRegex[Out: Arbitrary: Cogen]: Arbitrary[Regex[Char, Out]] =
    Arbitrary(genStandardCharRegex[Out])
}
