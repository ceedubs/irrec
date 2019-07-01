package ceedubs.irrec
package parse

import ceedubs.irrec.regex.RegexPrettyPrinter.{
  charClassCharsToEscape,
  nonCharClassCharsToEscape,
  specialNonCharClassCharToLit
}

import cats.implicits._
import fastparse._, NoWhitespace._
import ceedubs.irrec.regex.Match
import ceedubs.irrec.regex.Regex
import ceedubs.irrec.regex._
import cats.collections.{Diet, Range}
import ceedubs.irrec.regex.Match.MatchSet

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
    CharPred(specialNonCharClassCharToLit.contains(_)).!.map(s =>
      specialNonCharClassCharToLit(s.head))
      .opaque(
        s"special regular expression character that should be escaped such as '(', '}', '*', etc")

  /**
   * A shorthand class such as `\d` or `\w`. This parser itself doesn't look for the `\`; it starts
   * with the character after it.
   */
  def shorthandClass[_: P]: P[Match.MatchSet[Char]] =
    (
      P("d").map(_ => MatchSet.allow(CharacterClasses.digit)) |
        P("D").map(_ => MatchSet.forbid(CharacterClasses.digit)) |
        P("w").map(_ => MatchSet.allow(CharacterClasses.wordChar)) |
        P("W").map(_ => MatchSet.forbid(CharacterClasses.wordChar)) |
        P("h").map(_ => MatchSet.allow(CharacterClasses.horizontalWhitespaceChar)) |
        P("H").map(_ => MatchSet.forbid(CharacterClasses.horizontalWhitespaceChar)) |
        P("s").map(_ => MatchSet.allow(CharacterClasses.whitespaceChar)) |
        P("S").map(_ => MatchSet.forbid(CharacterClasses.whitespaceChar))
    ).opaque("""character class such as \w, \d, \s, \S, etc""")

  def negatedShorthandClass[_: P]: P[Match.MatchSet[Char]] =
    P("d").map(_ => MatchSet.forbid(CharacterClasses.digit)) |
    P("D").map(_ => MatchSet.allow(CharacterClasses.digit)) |
      P("w").map(_ => MatchSet.forbid(CharacterClasses.wordChar)) |
      P("W").map(_ => MatchSet.allow(CharacterClasses.wordChar)) |
      P("h").map(_ => MatchSet.forbid(CharacterClasses.horizontalWhitespaceChar)) |
      P("H").map(_ => MatchSet.allow(CharacterClasses.horizontalWhitespaceChar)) |
      P("s").map(_ => MatchSet.forbid(CharacterClasses.whitespaceChar)) |
      P("S").map(_ => MatchSet.allow(CharacterClasses.whitespaceChar))

  /**
   * Standard characters to match like `a` or `%`.
   */
  def standardMatchChar[_: P]: P[Char] =
    CharPred(c => !nonCharClassCharsToEscape.contains(c)).!.map(s => s.head)
      .opaque("""standard charact to match like `a` or `%`""")

  /**
   * Standard characters to match like `a` or `%` but also characters that aren't special within
   * character classes such as `*` (ex: `[*+]` matches on literal `*` and `+`).
   */
  def charClassStandardMatchChar[_: P]: P[Char] =
    P(CharPred(c => !charClassCharsToEscape.contains(c)).!.map(s => s.head))

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

  def singleLitCharClassChar[_: P]: P[Char] =
    P(("\\" ~ specialChar | charClassStandardMatchChar))

  def matchLitCharClassChar[_: P]: P[Match.Literal[Char]] =
    P(singleLitCharClassChar.map(Match.Literal(_)))

  /**
   * Character range like `a-z`.
   */
  def matchCharRange[_: P]: P[Range[Char]] = P(
    (singleLitCharClassChar ~ "-" ~/ singleLitCharClassChar).map {
      case (l, h) => Range(l, h)
    }
  )

  def negatedCharOrRange[_: P]: P[Match.MatchSet[Char]] =
    (matchCharRange.map(r => MatchSet.forbid(Diet.fromRange(r))) | singleLitCharClassChar.map(c => MatchSet.forbid(Diet.one(c))))

  def negatedPOSIXClass[_: P]: P[MatchSet[Char]] =
    P("alnum").map(_ => MatchSet.forbid(CharacterClasses.alphaNumeric)) |
      P("alpha").map(_ => MatchSet.forbid(CharacterClasses.alpha)) |
      P("ascii").map(_ => MatchSet.forbid(CharacterClasses.ascii)) |
      P("blank").map(_ => MatchSet.forbid(CharacterClasses.horizontalWhitespaceChar)) |
      P("cntrl").map(_ => MatchSet.forbid(CharacterClasses.controlChar)) |
      P("digit").map(_ => MatchSet.forbid(CharacterClasses.digit)) |
      P("graph").map(_ => MatchSet.forbid(CharacterClasses.graphChar)) |
      P("lower").map(_ => MatchSet.forbid(CharacterClasses.lowerAlpha)) |
      P("print").map(_ => MatchSet.forbid(CharacterClasses.printableChar)) |
      P("punct").map(_ => MatchSet.forbid(CharacterClasses.punctuationChar)) |
      P("space").map(_ => MatchSet.forbid(CharacterClasses.whitespaceChar)) |
      P("upper").map(_ => MatchSet.forbid(CharacterClasses.upperAlpha)) |
      P("word").map(_ => MatchSet.forbid(CharacterClasses.wordChar)) |
      P("xdigit").map(_ => MatchSet.forbid(CharacterClasses.hexDigit))

  def negatedCharClassContent[_: P]: P[MatchSet[Char]] =
    (
      ("\\" ~ negatedShorthandClass) |
        ("[:" ~/ negatedPOSIXClass ~ ":]") |
        negatedCharOrRange
    ).opaque(
        """literal character to match (ex: 'a'), escaped special character literal (ex: '\*'), a shorthand class (ex: '\w'), or a POSIX class (ex: '[:alpha:]')""")
      .rep(1)
      .map(_.reduceLeftOption(_ intersect _).get) // .get is safe because of .rep(1) but is gross

  def positivePOSIXCharClass[_: P]: P[MatchSet[Char]] =
    P("alnum").map(_ => MatchSet.allow(CharacterClasses.alphaNumeric)) |
      P("alpha").map(_ => MatchSet.allow(CharacterClasses.alpha)) |
      P("ascii").map(_ => MatchSet.allow(CharacterClasses.ascii)) |
      P("blank").map(_ => MatchSet.allow(CharacterClasses.horizontalWhitespaceChar)) |
      P("cntrl").map(_ => MatchSet.allow(CharacterClasses.controlChar)) |
      P("digit").map(_ => MatchSet.allow(CharacterClasses.digit)) |
      P("graph").map(_ => MatchSet.allow(CharacterClasses.graphChar)) |
      P("lower").map(_ => MatchSet.allow(CharacterClasses.lowerAlpha)) |
      P("print").map(_ => MatchSet.allow(CharacterClasses.printableChar)) |
      P("punct").map(_ => MatchSet.allow(CharacterClasses.punctuationChar)) |
      P("space").map(_ => MatchSet.allow(CharacterClasses.whitespaceChar)) |
      P("upper").map(_ => MatchSet.allow(CharacterClasses.upperAlpha)) |
      P("word").map(_ => MatchSet.allow(CharacterClasses.wordChar)) |
      P("xdigit").map(_ => MatchSet.allow(CharacterClasses.hexDigit))

  def positiveCharClassContent[_: P]: P[MatchSet[Char]] =
    (
      ("\\" ~ shorthandClass) |
        ("[:" ~/ positivePOSIXCharClass ~ ":]") |
        (matchCharRange.map(r => MatchSet.allow(Diet.fromRange(r))) | singleLitCharClassChar.map(c => MatchSet.allow(Diet.one(c))))
    ).opaque(
        """literal character to match (ex: 'a'), escaped special character literal (ex: '\*'), a shorthand class (ex: '\w'), or a POSIX class (ex: '[:alpha:]')""")
      .rep(1)
      .map(_.reduceOption(_ union _).get) // .get is safe because of .rep(1), but this is gross

  /**
   * Character classes like `[acz]` or `[^a-cHP-W]`.
   */
  def charClassTerm[_: P]: P[MatchSet[Char]] =
    P(
      "[" ~
        (("^" ~/ negatedCharClassContent) | positiveCharClassContent) ~/
        "]")

  // TODO ceedubs handle unions
  // TODO ceedubs I don't know if this is getting precedence right.
  def compositeCharacterClass[_: P]: P[MatchSet[Char]] = charClassTerm.flatMap{ m =>
    ("&&" ~/ compositeCharacterClass).map(m2 => m.intersect(m2)) |
      Pass(m)
  }

  def charClass[_: P]: P[Regex[Char]] = (("[" ~ compositeCharacterClass ~ "]") | charClassTerm).map(Regex.matching(_))

  def base[_: P]: P[Regex[Char]] = P(
    standardMatchChar.map(Regex.lit(_)) |
      ("\\" ~/ (specialChar.map(Regex.lit(_)) | shorthandClass.map(Regex.matching(_)))) |
      wildcard |
      charClass |
      ("(" ~/ "?:".? ~ regex ~ ")")
  )

  def factor[_: P]: P[Regex[Char]] = P {
    base.flatMap { r =>
      P("*").map(_ => r.star) |
        P("+").map(_ => r.oneOrMore) |
        P("?").map(_ => r.optional) |
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
