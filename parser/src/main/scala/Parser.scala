package ceedubs.irrec
package parse

import ceedubs.irrec.regex.RegexPrettyPrinter.charsToEscape

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

    def max: Int = this match {
      case RepeatCount.Exact(n) => n
      case RepeatCount.Range(_, upper) => upper
    }
  }

  object RepeatCount {
    final case class Exact(n: Int) extends RepeatCount
    final case class Range(lowerInclusive: Int, upperInclusive: Int) extends RepeatCount
  }

  /**
   * Matches on escaped special characters like `\*` and `\{`.
   */
  def escapedChar[_: P]: P[Char] =
    P("\\" ~/ CharPred(charsToEscape.contains(_)).!.map(_.head))

  /**
   * Standard characters to match like `a` or `%`.
   */
  def standardMatchChar[_: P]: P[Char] =
    P(CharPred(c => !charsToEscape.contains(c)).!.map(s => s.head))

  def singleLitChar[_: P]: P[Char] =
    P((escapedChar | standardMatchChar))

  def matchLitChar[_: P]: P[Match.Literal[Char]] =
    P(singleLitChar.map(Match.Literal(_)))

  def litChar[_: P]: P[Regex[Char]] = P(singleLitChar.map(Regex.lit))

  /**
   * Matches the wildcard character `.`.
   */
  def wildcard[_: P]: P[Regex[Char]] = P(".").map(_ => Regex.wildcard)

  /**
   * Positive integers within the max range of Scala's `Int`.
   */
  def posInt[_: P]: P[Int] = P(CharIn("0-9").rep(1).!.flatMap{ s =>
    Either.catchNonFatal(s.toInt).fold(_ => Fail, Pass(_))
  }).opaque(s"<Integer between 0 and ${Int.MaxValue}>")

  /**
   * Matches repeat counts like `{3}` or `{1,4}`.
   */
  def repeatCount[_: P]: P[RepeatCount] = P(
    "{" ~/ (
      (posInt ~ "," ~/ posInt).map{ case (l, h) => RepeatCount.Range(l, h) } |
      posInt.map(RepeatCount.Exact(_))
    ) ~ "}")

  /**
   * Character range like `a-z`.
   */
  def matchCharRange[_: P]: P[Match.Range[Char]] = P(
    (singleLitChar ~ "-" ~/ singleLitChar).map{ case (l, h) =>
      Match.Range(l, h)
    }
  )

  /**
   * Character classes like `[acz]` or `[a-cHP-W]`.
   */
  def charClass[_: P]: P[Regex[Char]] =
    P("[" ~/ (matchCharRange | matchLitChar).rep(1) ~ "]").map(matches =>
      Regex.oneOfR(Coattr.pure(matches.head), matches.tail.map(Coattr.pure[KleeneF, Match[Char]](_)): _*))

  def base[_: P]: P[Regex[Char]] = P(("(" ~/ regex ~ ")") | litChar | wildcard | charClass)

  def factor[_: P]: P[Regex[Char]] = P {
    base.flatMap{ r =>
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
    term.flatMap{ r1 =>
      ("|" ~/ regex).map(r2 => r1 | r2) |
      Pass(r1)
    }
  )

  /**
   * A parser for strings that are complete regular expressions, up until the end of the string.
   */
  def regexExpr[_: P]: P[Regex[Char]] = P(regex ~ End)
}
