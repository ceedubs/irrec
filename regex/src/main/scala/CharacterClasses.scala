package ceedubs.irrec
package regex

import cats.collections.{Diet, Range}
import cats.implicits._

object CharacterClasses {

  val digit: Diet[Char] = Diet.fromRange(Range('0', '9'))

  val lowerAlpha: Diet[Char] =
    Diet.fromRange(Range('a', 'z'))

  val upperAlpha: Diet[Char] =
    Diet.fromRange(Range('A', 'Z'))

  val alpha: Diet[Char] =
    upperAlpha | lowerAlpha

  val alphaNumeric: Diet[Char] =
    digit | alpha

  val hexDigit: Diet[Char] =
    Diet.fromRange(Range('A', 'F')) + Range('a', 'f') + Range('0', '9')

  val ascii: Diet[Char] = Diet.fromRange(Range('\u0000', '\u007F'))

  val wordChar: Diet[Char] =
    alphaNumeric + '_'

  val horizontalWhitespaceChar: Diet[Char] =
    Diet.one('\t') + ' '

  val whitespaceChar: Diet[Char] = horizontalWhitespaceChar + '\n' + '\f' + '\r' + '\u000B'

  val controlChar: Diet[Char] = Diet.fromRange(Range('\u0000', '\u001F')) + '\u007F'

  val graphChar: Diet[Char] = Diet.fromRange(Range('\u0021', '\u007E'))

  val printableChar: Diet[Char] = Diet.fromRange(Range('\u0020', '\u007E'))

  val punctuationChar: Diet[Char] =
    Diet
      .fromRange(Range('\u0021', '\u002F'))
      .addRange(Range('\u003A', '\u0040'))
      .addRange(Range('\u005B', '\u0060'))
      .addRange(Range('\u007B', '\u007E'))
}
