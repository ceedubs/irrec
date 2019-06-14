package ceedubs.irrec
package regex

import Match._

import cats.data.NonEmptyList

object CharacterClasses {

  val digitMatch: Range[Char] = Range('0', '9')

  val nonDigitMatch: Negated.NegatedRange[Char] = digitMatch.negate

  val lowerAlphaMatch: Match.Range[Char] =
    Range('a', 'z')

  val nonLowerAlphaMatch: Negated.NegatedRange[Char] =
    lowerAlphaMatch.negate

  val upperAlphaMatch: Match.Range[Char] =
    Range('A', 'Z')

  val nonUpperAlphaMatch: Negated.NegatedRange[Char] =
    upperAlphaMatch.negate

  val alphaMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(upperAlphaMatch, lowerAlphaMatch)

  val nonAlphaMatches: NonEmptyList[Match.Negated[Char]] =
    alphaMatches.map(_.negate)

  val alphaNumericMatches: NonEmptyList[Match.Negatable[Char]] =
    digitMatch :: alphaMatches

  val nonAlphaNumericMatches: NonEmptyList[Match.Negated[Char]] =
    alphaNumericMatches.map(_.negate)

  val hexDigitMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(Range('A', 'F'), Range('a', 'f'), Range('0', '9'))

  val nonHexDigitMatches: NonEmptyList[Match.Negated[Char]] =
    hexDigitMatches.map(_.negate)

  val asciiMatch: Match.Range[Char] = Range('\u0000', '\u007F')

  val nonAsciiMatch: Match.Negated[Char] =
    asciiMatch.negate

  val wordCharMatches: NonEmptyList[Match.Negatable[Char]] =
    Literal('_') :: alphaNumericMatches

  val nonWordCharMatches: NonEmptyList[Match.Negated[Char]] =
    wordCharMatches.map(_.negate)

  val horizontalWhitespaceCharMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(Literal('\t'), Literal(' '))

  val nonHorizontalWhitespaceCharMatches: NonEmptyList[Match.Negated[Char]] =
    horizontalWhitespaceCharMatches.map(_.negate)

  val whitespaceCharMatches: NonEmptyList[Match.Negatable[Char]] =
    horizontalWhitespaceCharMatches concatNel
      NonEmptyList.of(Literal('\n'), Literal('\f'), Literal('\r'), Literal('\u000B'))

  val nonWhitespaceCharMatches: NonEmptyList[Match.Negated[Char]] =
    whitespaceCharMatches.map(_.negate)

  val controlCharMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(Range('\u0000', '\u001F'), Literal('\u007F'))

  val nonControlCharMatches: NonEmptyList[Match.Negated[Char]] =
    controlCharMatches.map(_.negate)

  val graphCharMatch: Range[Char] =
    Range('\u0021', '\u007E')

  val nonGraphCharMatch: Negated.NegatedRange[Char] =
    graphCharMatch.negate

  val printableCharMatch: Range[Char] =
    Range('\u0020', '\u007E')

  val nonPrintableCharMatch: Negated.NegatedRange[Char] =
    printableCharMatch.negate

  val punctuationCharMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(
      Range('\u0021', '\u002F'),
      Range('\u003A', '\u0040'),
      Range('\u005B', '\u0060'),
      Range('\u007B', '\u007E'))

  val nonPunctuationCharMatches: NonEmptyList[Match.Negated[Char]] =
    punctuationCharMatches.map(_.negate)
}
