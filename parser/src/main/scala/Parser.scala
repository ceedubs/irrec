package ceedubs.irrec
package parse

import ceedubs.irrec.regex.RegexPrettyPrinter.{charsToEscape, specialCharToLit}

import cats.data.NonEmptyList
import cats.implicits._
import fastparse._, NoWhitespace._
import ceedubs.irrec.regex.Match
import ceedubs.irrec.regex.Regex
import ceedubs.irrec.regex._
import qq.droste.data.Coattr

object Parser {
  sealed abstract class RepeatCount extends Product with Serializable {
    def min: Int = this match {
      case RepeatCount.Exact(n) => n
      case RepeatCount.Range(lower, _) => lower
    }

    def max: Option[Int] = this match {
      case RepeatCount.Exact(n) => Some(n)
      case RepeatCount.Range(_, upper) => upper
    }
  }

  object RepeatCount {
    final case class Exact(n: Int) extends RepeatCount
    final case class Range(lowerInclusive: Int, upperInclusive: Option[Int]) extends RepeatCount
  }

  /**
   * Matches on special characters that should be escaped like `*` and `{`.
   */
  def specialChar[_: P]: P[Char] =
    P(CharPred(specialCharToLit.contains(_)).!.map(s => specialCharToLit(s.head))
      .opaque(
        s"special regular expression character that should be escaped such as '(', '}', '*', etc"))

  /**
   * A shorthand class such as `\d` or `\w`. This parser itself doesn't look for the `\`; it starts
   * with the character after it.
   */
  def shorthandClass[_: P]: P[Regex[Char]] =
    (
      P("d").map(_ => Regex.digit) |
        P("D").map(_ => Regex.nonDigit) |
        P("w").map(_ => Regex.wordCharacter) |
        P("W").map(_ => Regex.nonWordCharacter) |
        P("h").map(_ => Regex.horizontalWhitespaceCharacter) |
        P("H").map(_ => Regex.notHorizontalWhitespaceCharacter) |
        P("s").map(_ => Regex.whitespaceCharacter) |
        P("S").map(_ => Regex.nonWhitespaceCharacter)
    ).opaque("""character class such as \w, \d, \s, \S, etc""")

  def negatedShorthandClass[_: P]: P[NonEmptyList[Match.Negated[Char]]] =
    P("d").map(_ => NonEmptyList.one(CharacterClasses.digitMatch.negate)) |
      P("w").map(_ => CharacterClasses.wordCharMatches.map(_.negate)) |
      P("s").map(_ => CharacterClasses.whitespaceCharMatches.map(_.negate))

  /**
   * Standard characters to match like `a` or `%`.
   */
  def standardMatchChar[_: P]: P[Char] =
    P(CharPred(c => !charsToEscape.contains(c)).!.map(s => s.head))

  /**
   * Matches the wildcard character `.`.
   */
  def wildcard[_: P]: P[Regex[Char]] = P(".").map(_ => Regex.wildcard)

  /**
   * Positive integers within the max range of Scala's `Int`.
   */
  def posInt[_: P]: P[Int] =
    P(CharIn("0-9").rep(1).!.flatMap { s =>
      Either.catchNonFatal(s.toInt).fold(_ => Fail, Pass(_))
    }).opaque(s"integer between 0 and ${Int.MaxValue}")

  /**
   * Matches repeat counts like `{3}` or `{1,4}`.
   */
  def repeatCount[_: P]: P[RepeatCount] =
    P(
      "{" ~/ (
        (posInt ~ "," ~/ posInt.?).map { case (l, h) => RepeatCount.Range(l, h) } |
          posInt.map(RepeatCount.Exact(_))
      ) ~ "}").opaque("repeat count such as '{3}', '{1,4}', or '{3,}'")

  def singleLitChar[_: P]: P[Char] =
    P(("\\" ~ specialChar | standardMatchChar))

  def matchLitChar[_: P]: P[Match.Literal[Char]] =
    P(singleLitChar.map(Match.Literal(_)))

  /**
   * Character range like `a-z`.
   */
  def matchCharRange[_: P]: P[Match.Range[Char]] = P(
    (singleLitChar ~ "-" ~/ singleLitChar).map {
      case (l, h) =>
        Match.Range(l, h)
    }
  )

  def negatedCharOrRange[_: P]: P[Match.Negated[Char]] =
    (matchCharRange.map(Match.Negated.NegatedRange(_)) | matchLitChar.map(
      Match.Negated.NegatedLiteral(_)))

  def negatedCharClassContent[_: P]: P[Match.NoneOf[Char]] =
    (negatedCharOrRange.map(NonEmptyList.one(_)) | ("\\" ~/ negatedShorthandClass))
      .rep(1)
      .map(xs => Match.NoneOf(xs.reduceOption(_ |+| _).get)) // .get is safe because of .rep(1)

  def positiveCharClassContent[_: P]: P[Regex[Char]] =
    (
      (matchCharRange | matchLitChar).map(Coattr.pure[KleeneF, Match[Char]](_)) |
        ("\\" ~/ shorthandClass)
    ).opaque(
        """literal character to match (ex: 'a'), escaped special character literal (ex: '\*'), or shorthand class (ex: '\w')""")
      .rep(1)
      .map(matches => Regex.oneOfR(matches.head, matches.tail: _*)) // .head is safe because of .rep(1)

  /**
   * Character classes like `[acz]` or `[^a-cHP-W]`.
   */
  def charClass[_: P]: P[Regex[Char]] =
    P(
      "[" ~/
        (("^" ~/ negatedCharClassContent
          .map(Coattr.pure[KleeneF, Match[Char]](_))) | positiveCharClassContent) ~/
        "]")

  def base[_: P]: P[Regex[Char]] = P(
    standardMatchChar.map(Regex.lit(_)) |
      ("\\" ~/ (specialChar.map(Regex.lit(_)) | shorthandClass)) |
      wildcard |
      charClass |
      ("(" ~/ "?:".? ~ regex ~ ")")
  )

  def factor[_: P]: P[Regex[Char]] = P {
    base.flatMap { r =>
      P("*").map(_ => r.star) |
        P("+").map(_ => r.oneOrMore) |
        repeatCount.map(count => r.repeat(count.min, count.max)) |
        Pass(r)
    }
  }

  def term[_: P]: P[Regex[Char]] = P(factor.rep(0).map(factors => Regex.allOfR(factors: _*)))

  /**
   * A parser for a regular expression. You probably want to use [[regexExpr]] instead, as this
   * parser will succeed even if there are trailing characters after a valid regular expression.
   */
  def regex[_: P]: P[Regex[Char]] = P(
    term.flatMap { r1 =>
      ("|" ~/ regex).map(r2 => r1 | r2) |
        Pass(r1)
    }
  )

  /**
   * A parser for strings that are complete regular expressions, up until the end of the string.
   */
  def regexExpr[_: P]: P[Regex[Char]] = P(regex ~ End)
}
