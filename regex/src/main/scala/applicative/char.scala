package ceedubs.irrec
package regex
package applicative

import Regex._

import cats.implicits._

object char {
  /**
   * Matches a single digit character ('0', '3', '9', etc). Could be represented in a regular
   * expression as `\d` or `[0-9]`.
   */
  val digit: RegexC[Int] = inSet(CharacterClasses.digit).map(_.toInt)

  /**
   * Opposite of [[digit]]. Could be represented in a regular expression as
   * `\D`.
   */
  val nonDigit: RegexC[Char] =
    notInSet(CharacterClasses.digit)

  /**
   * Matches a single lowercase character ('a', 'z', etc). Could be represented in a regular
   * expression as `[a-z]` or `[:lower:]`.
   */
  val lowerAlphaChar: RegexC[Char] = inSet(CharacterClasses.lowerAlpha)

  /**
   * Opposite of [[lowerAlphaChar]]. Could be represented in a regular expression as
   * `[^a-z]` or `[^[:lower:]]`.
   */
  val nonLowerAlphaChar: RegexC[Char] = notInSet(CharacterClasses.lowerAlpha)

  /**
   * Matches a single uppercase character ('a', 'z', etc). Could be represented in a regular
   * expression as `[a-z]` or `[:upper:]`.
   */
  val upperAlphaChar: RegexC[Char] = inSet(CharacterClasses.upperAlpha)

  /**
   * Opposite of [[upperAlphaChar]]. Could be represented in a regular expression as
   * `[^a-z]` or `[^[:upper:]]`.
   */
  val nonUpperAlphaChar: RegexC[Char] = notInSet(CharacterClasses.upperAlpha)

  /**
   * Matches a single alphabetic character ('a', 'A', etc). Could be represented in a regular
   * expression as `[:alpha:]`.
   */
  val alphaChar: RegexC[Char] = inSet(CharacterClasses.alpha)

  /**
   * Opposite of [[alphaChar]]. Could be represented in a regular expression as
   * `[^[:alalpha:]]`.
   */
  val nonAlphaChar: RegexC[Char] = notInSet(CharacterClasses.alpha)

  /**
   * Matches a single alphanumeric character ('0', 'a', 'A', etc). Could be represented in a regular
   * expression as `[:alnum:]`.
   */
  val alphaNumericChar: RegexC[Char] = inSet(CharacterClasses.alphaNumeric)

  /**
   * Opposite of [[alphaNumericChar]]. Could be represented in a regular expression as
   * `[^[:alnum:]]`.
   */
  val nonAlphaNumericChar: RegexC[Char] = notInSet(CharacterClasses.alphaNumeric)

  /**
   * Matches a single hexadecimal digit ('0', '1', 'A', 'F', 'a', 'f', etc). Could be represented in
   * a regular expression as `[:xdigit:]`.
   */
  val hexDigitChar: RegexC[Char] = inSet(CharacterClasses.hexDigit)

  /**
   * Opposite of [[hexDigitChar]]. Could be represented in a regular expression as
   * `[^[:alnum:]]`.
   */
  val nonHexDigitChar: RegexC[Char] = notInSet(CharacterClasses.hexDigit)

  /**
   * Matches a single "word" character ('A', 'a', '_', etc). Could be represented in a regular
   * expression as `\w`.
   */
  val wordChar: RegexC[Char] = inSet(CharacterClasses.wordChar)

  /**
   * Opposite of [[wordChar]]. Could be represented in a regular expression as
   * `\W`.
   */
  val nonWordChar: RegexC[Char] = notInSet(CharacterClasses.wordChar)

  /**
   * A single horizontal whitespace character `[\t ]`. Could be represented in a regular expression
   * as `\h`.
   */
  val horizontalWhitespaceChar: RegexC[Char] = inSet(CharacterClasses.horizontalWhitespaceChar)

  /**
   * Opposite of [[horizontalWhitespaceChar]]; this matches on any character that is not a tab
   * or a space. Could be represented in a regular expression as `\H`.
   */
  val nonHorizontalWhitespaceChar: RegexC[Char] = notInSet(CharacterClasses.horizontalWhitespaceChar)

  /**
   * A single whitespace character `[\t\n\f\r ]`. Could be represented in a regular expression as
   * `\s`.
   */
  val whitespaceChar: RegexC[Char] = inSet(CharacterClasses.whitespaceChar)

  /**
   * Opposite of [[whitespaceChar]]. Could be represented in a regular expression as
   * `\S`.
   */
  val nonWhitespaceChar: RegexC[Char] = notInSet(CharacterClasses.whitespaceChar)

  /**
   * A single ASCII character `[ -~]`. Could be represented in a regular expression as
   * `[:ascii:]`.
   */
  val asciiChar: RegexC[Char] = inSet(CharacterClasses.ascii)

  /**
   * Opposite of [[asciiChar]]. Could be represented in a regular expression as
   * `[^[:ascii:]]`.
   */
  val nonAsciiChar: RegexC[Char] = notInSet(CharacterClasses.ascii)

  /**
   * A single control character `[\x00-\x1F\x7F]`. Could be represented in a regular expression as
   * `[:cntrl:]`.
   */
  val controlChar: RegexC[Char] = inSet(CharacterClasses.controlChar)

  /**
   * Opposite of [[controlChar]]. Could be represented in a regular expression as
   * `[^[:cntrl:]]`.
   */
  val nonControlChar: RegexC[Char] = notInSet(CharacterClasses.controlChar)

  /**
   * A single visible (graphical) character `[\x21-\x7E]`. Could be represented in a regular
   * expression as `[:graph:]`.
   */
  val graphChar: RegexC[Char] = inSet(CharacterClasses.graphChar)

  /**
   * Opposite of [[graphChar]]. Could be represented in a regular expression as `[^[:graph:]]`.
   */
  val nonGraphChar: RegexC[Char] = notInSet(CharacterClasses.graphChar)

  /**
   * A single printable character (visible character or space). Could be represented in a regular
   * expression as `[:print:]` or `\x20-\x7E`.
   */
  val printableChar: RegexC[Char] = inSet(CharacterClasses.printableChar)

  /**
   * Opposite of [[printableChar]]. Could be represented in a regular expression as `[^[:print:]]`.
   */
  val nonPrintableChar: RegexC[Char] = notInSet(CharacterClasses.printableChar)

  /**
   * A single punctuation character (`;`, `!`, etc).. Could be represented in a regular expression
   * as `[:punct:]`.
   */
  val punctuationChar: RegexC[Char] = inSet(CharacterClasses.punctuationChar)

  /**
   * Opposite of [[punctuationChar]]. Could be represented in a regular expression as
   * `[^[:punct:]]`.
   */
  val nonPunctuationChar: RegexC[Char] = notInSet(CharacterClasses.punctuationChar)
}
