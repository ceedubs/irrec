package ceedubs.irrec
package regex.applicative
// TODO package

// TODO
import Regex.Regex
import ceedubs.irrec.regex.{CharacterClasses, CharRegexGen => CharRegexGenOld}
import ceedubs.irrec.regex.{RegexGen => RegexGenOld}

import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.implicits._
import cats.collections.{Diet, Range}

object CharRegexGen {
  val supportedCharacters: Diet[Char] =
    Diet.fromRange(Range('\u0000', '\uD7FF')).addRange(Range('\uF900', '\uFFFD'))

  def genSupportedCharRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(CharRegexGenOld.standardCharRegexGenConfig)

  def genAlphaNumRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(RegexGenOld.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric))

  def genAsciiRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] =
    RegexGen.genRegex(RegexGenOld.Config.fromDiscreteDiet(CharacterClasses.ascii))

  def genStandardCharRegex[Out: Arbitrary: Cogen]: Gen[Regex[Char, Out]] = Gen.frequency(
    5 -> genAsciiRegex[Out],
    4 -> genAlphaNumRegex[Out],
    1 -> genSupportedCharRegex[Out]
  )

  implicit def arbCharRegex[Out: Arbitrary: Cogen]: Arbitrary[Regex[Char, Out]] =
    Arbitrary(genStandardCharRegex[Out])
}
