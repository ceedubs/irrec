package ceedubs.irrec
package regex

import Match._

import cats.implicits._

object CharacterClasses {

  val digitMatch: MatchSet[Char] = MatchSet.range('0', '9')

  val nonDigitMatch: NegatedMatchSet[Char] = digitMatch.negate

  val lowerAlphaMatch: MatchSet[Char] =
    MatchSet.range('a', 'z')

  val nonLowerAlphaMatch: NegatedMatchSet[Char] =
    lowerAlphaMatch.negate

  val upperAlphaMatch: MatchSet[Char] =
    MatchSet.range('A', 'Z')

  val nonUpperAlphaMatch: NegatedMatchSet[Char] =
    upperAlphaMatch.negate

  val alphaMatches: MatchSet[Char] =
    upperAlphaMatch union lowerAlphaMatch

  val nonAlphaMatches: NegatedMatchSet[Char] =
    alphaMatches.negate

  val alphaNumericMatches: MatchSet[Char] =
    digitMatch union alphaMatches

  val nonAlphaNumericMatches: NegatedMatchSet[Char] =
    alphaNumericMatches.negate

  val hexDigitMatches: MatchSet[Char] =
    MatchSet.range('A', 'F') union MatchSet.range('a', 'f') union MatchSet.range('0', '9')

  // TODO ceedubs is there even any value in all of these negated match sets?
  // They don't seem to be very relevant at this point.
  val nonHexDigitMatches: NegatedMatchSet[Char] =
    hexDigitMatches.negate

  val asciiMatch: MatchSet[Char] = MatchSet.range('\u0000', '\u007F')

  val nonAsciiMatch: NegatedMatchSet[Char] =
    asciiMatch.negate

  val wordCharMatches: MatchSet[Char] =
    alphaNumericMatches union MatchSet.one('_')

  val nonWordCharMatches: NegatedMatchSet[Char] =
    wordCharMatches.negate

  val horizontalWhitespaceCharMatches: MatchSet[Char] =
    MatchSet.one('\t') union MatchSet.one(' ')

  val nonHorizontalWhitespaceCharMatches: NegatedMatchSet[Char] =
    horizontalWhitespaceCharMatches.negate

  val whitespaceCharMatches: MatchSet[Char] =
    horizontalWhitespaceCharMatches
    .union(MatchSet.one('\n'))
    .union(MatchSet.one('\f'))
    .union(MatchSet.one('\r'))
    .union(MatchSet.one('\u000B'))

  val nonWhitespaceCharMatches: NegatedMatchSet[Char] =
    whitespaceCharMatches.negate

  val controlCharMatches: MatchSet[Char] =
    MatchSet.range('\u0000', '\u001F')
    .union(MatchSet.one('\u007F'))

  val nonControlCharMatches: NegatedMatchSet[Char] =
    controlCharMatches.negate

  val graphCharMatch: MatchSet[Char] =
    MatchSet.range('\u0021', '\u007E')

  val nonGraphCharMatch: NegatedMatchSet[Char] =
    graphCharMatch.negate

  val printableCharMatch: MatchSet[Char] =
    MatchSet.range('\u0020', '\u007E')

  val nonPrintableCharMatch: NegatedMatchSet[Char] =
    printableCharMatch.negate

  // TODO ceedubs remove the "matches" suffix from a lot of these names?
  val punctuationCharMatches: MatchSet[Char] =
      MatchSet.range('\u0021', '\u002F')
      .union(MatchSet.range('\u003A', '\u0040'))
      .union(MatchSet.range('\u005B', '\u0060'))
      .union(MatchSet.range('\u007B', '\u007E'))

  val nonPunctuationCharMatches: NegatedMatchSet[Char] =
    punctuationCharMatches.negate
}
