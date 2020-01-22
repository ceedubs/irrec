package ceedubs.irrec
package regex

import cats.{~>, Eq}
import cats.data.NonEmptyList
import cats.collections.Diet
import cats.implicits._

object RegexPrettyPrinter {
  private val andThenPrecedence: Int = 2
  private val orPrecedence: Int = 3
  private val starPrecedence: Int = 1
  private val epsPrecedence: Int = 0
  private val failPrecedence: Int = 0
  private val matchPrecedence: Int = 0

  val nonCharClassCharsToEscape: Set[Char] =
    Set('<', '(', '[', '{', '\\', '^', '=', '$', '!', '|', ']', '}', ')', '?', '*', '+', '.', '>')

  val charClassCharsToEscape: Set[Char] = Set('[', ']', '\\', '^', '-')

  val whitespaceCharMappings: Map[Char, Char] =
    Map('t' -> '\t', 'n' -> '\n', 'r' -> '\r', 'f' -> '\f')

  val specialNonCharClassCharToLit: Map[Char, Char] =
    whitespaceCharMappings ++ nonCharClassCharsToEscape.map(x => (x, x))

  val specialCharClassCharToLit: Map[Char, Char] =
    whitespaceCharMappings ++ charClassCharsToEscape.map(x => (x, x))

  val nonCharClassCharToEscapedChar: Map[Char, String] =
    specialNonCharClassCharToLit.map {
      case (special, lit) =>
        (lit, "\\" + special)
    }

  val charClassCharToEscapedChar: Map[Char, String] =
    specialCharClassCharToLit.map {
      case (special, lit) =>
        (lit, "\\" + special)
    }

  def nonGraphicalToUnicode(c: Char): String =
    if (CharacterClasses.graphChar.contains(c)) c.toString
    else f"\\u${c.toInt}%04x"

  def showChar(inCharacterClass: Boolean): Char => String =
    if (inCharacterClass) c => charClassCharToEscapedChar.get(c).getOrElse(nonGraphicalToUnicode(c))
    else c => nonCharClassCharToEscapedChar.get(c).getOrElse(nonGraphicalToUnicode(c))

  /**
   * @param f a function that takes an `A` value and a boolean indicating whether or not the `A` is
   * appearing within a range and formats it as a string.
   */
  def showMatch[A](f: (Boolean, A) => String)(implicit eqA: Eq[A]): Match[A] => String = {
    import Match._

    def showDiet(diet: Diet[A]): String =
      diet.foldLeftRange("") {
        case (s, cats.collections.Range(l, h)) =>
          val current = if (l === h) f(true, l) else s"${f(true, l)}-${f(true, h)}"
          s + current
      }

    _ match {
      case Literal(a) => f(false, a)
      case MatchSet.Allow(allowed) =>
        if (allowed.isEmpty) pprint(combinator.fail) else s"[${showDiet(allowed)}]"
      case MatchSet.Forbid(forbidden) =>
        if (forbidden.isEmpty) "." else s"[^${showDiet(forbidden)}]"
      case Match.Wildcard() => "."
    }
  }

  def parensMaybe(
    currentPrecedence: Int,
    value: (Int, String),
    parensForEqualPrecedence: Boolean): String =
    if (value._1 > currentPrecedence || (parensForEqualPrecedence && value._1 === currentPrecedence))
      s"(?:${value._2})"
    else value._2

  // TODO documentation
  // TODO generalize to not be specific to Match?
  // TODO this formatting is horrendous
  def pprintWith[In](f: (Boolean, In) => String)(
    implicit eqA: cats.Eq[In]): Regex[In, Match[In], _] => String = {
    val showMatch = regex.RegexPrettyPrinter.showMatch(f)
    def go[Out](r: Regex[In, Match[In], Out]): (Int, String) =
      Regex.fold[In, Match[In], Out, (Int, String)](
        eps = _ => (epsPrecedence, ""),
        fail = () => (failPrecedence, "∅"),
        elem = (m, _) => (matchPrecedence, showMatch(m)),
        andThen = λ[λ[i => (Regex[In, Match[In], i => Out], Regex[In, Match[In], i])] ~> λ[a => (
          Int,
          String)]](
          t =>
            (
              andThenPrecedence,
              parensMaybe(andThenPrecedence, go(t._1), false) + parensMaybe(
                andThenPrecedence,
                go(t._2),
                false))),
        star = λ[λ[i => (Regex[In, Match[In], i], Greediness, Out, (Out, i) => Out)] ~> λ[a => (
          Int,
          String)]](t => (starPrecedence, parensMaybe(starPrecedence, go(t._1), true) + "*")),
        mapped =
          λ[λ[a => (Regex[In, Match[In], a], a => Out)] ~> λ[a => (Int, String)]](t => go(t._1)),
        or = {
          case NonEmptyList(r, Nil) => go(r)
          case alternatives =>
            (
              orPrecedence,
              alternatives.map(r => parensMaybe(orPrecedence, go(r), false)).mkString_("|"))
        },
        void = _ => λ[Regex[In, Match[In], ?] ~> λ[a => (Int, String)]](go(_))
      )(r)
    go(_)._2
  }

  val pprint: Regex[Char, regex.Match[Char], _] => String = {
    pprintWith((inRange, c) => regex.RegexPrettyPrinter.showChar(inRange)(c))
  }
}
