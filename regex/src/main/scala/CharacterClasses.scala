package ceedubs.irrec
package regex

import Match._

import cats.data.NonEmptyList

object CharacterClasses {

  val digitMatch: Match.Range[Char] = Range('0', '9')

  val wordCharMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(Literal('_'), Range('A', 'Z'), Range('a', 'z'), Range('0', '9'))

  val horizontalWhitespaceCharMatches: NonEmptyList[Match.Negatable[Char]] =
    NonEmptyList.of(Literal('\t'), Literal(' '))

  val whitespaceCharMatches: NonEmptyList[Match.Negatable[Char]] =
    horizontalWhitespaceCharMatches concatNel
      NonEmptyList.of(Literal('\n'), Literal('\f'), Literal('\r'))
}
