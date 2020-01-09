package ceedubs.irrec
package parse

import ceedubs.irrec.regex.applicative.Regex
import ceedubs.irrec.regex.applicative.Regex.Regex
import Parser._

import fastparse._, NoWhitespace._
import fastparse.Parsed.{Failure, Success}
import cats.implicits._
import ceedubs.irrec.regex.applicative.Greediness

object Parser2 {
  /**
   * Matches the wildcard character `.`.
   */
  def wildcard[_: P]: P[Regex[Char, Char]] = P(".").map(_ => Regex.wildcard)

  // TODO
  def base[_: P]: P[Regex[Char, Unit]] = P(
    standardMatchChar.map(Regex.lit(_).void) |
      ("\\" ~/ (("u" ~ unicodeCodePoint | specialChar).map(Regex.lit(_).void) | shorthandClass.map(
        Regex.matching(_).void))) |
      wildcard.map(_.void) |
      charClass.map(Regex.matching(_).void) |
      ("(?:" ~ regex ~ ")")
  )

  def factor[_: P]: P[Regex[Char, Unit]] = P {
    base.flatMap { r =>
      // TODO greediness
      // TODO voids
      P("*").map(_ => r.star(Greediness.Greedy).void) |
        P("+").map(_ => r.oneOrMore(Greediness.Greedy).void) |
        P("?").map(_ => r.optional.void) |
        repeatCount.map(count => r.repeat(count.min, count.max, Greediness.Greedy).void) |
        Pass(r)
    }
  }

  // TODO can probably do better than toList call. Do we care?
  def term[_: P]: P[Regex[Char, Unit]] = P(factor.rep(0).map(_.toList.sequence_))

  /**
   * A parser for a regular expression. You probably want to use [[regexExpr]] instead, as this
   * parser will succeed even if there are trailing characters after a valid regular expression.
   */
  def regex[_: P]: P[Regex[Char, Unit]] = P(
    term.flatMap { r1 =>
      ("|" ~/ regex).map(r2 => r1 | r2) |
        Pass(r1)
    }
  )

  /**
   * A parser for strings that are complete regular expressions, up until the end of the string.
   */
  def regexExpr[_: P]: P[Regex[Char, Unit]] = P(regex ~ End)

  def parseRegex(regex: String): Either[String, Regex[Char, Unit]] =
    parse(regex, regexExpr(_), verboseFailures = true) match {
      case f @ Failure(_, _, _) => Left(f.msg)
      case Success(value, _) => Right(value)
    }
}
