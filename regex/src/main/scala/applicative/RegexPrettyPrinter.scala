package ceedubs.irrec
package regex
package applicative

import cats.~>
import cats.implicits._

// TODO name
object RegexPrettyPrinter {
  private val andThenPrecedence: Int = 2
  private val orPrecedence: Int = 3
  private val starPrecedence: Int = 1
  private val epsPrecedence: Int = 0
  private val failPrecedence: Int = 0
  private val matchPrecedence: Int = 0

  def parensMaybe(
    currentPrecedence: Int,
    value: (Int, String),
    parensForEqualPrecedence: Boolean): String =
    if (value._1 > currentPrecedence || parensForEqualPrecedence && value._1 === currentPrecedence)
      s"(?:${value._2})"
    else value._2

  // TODO naming
  // TODO documentation
  // TODO generalize to not be specific to Match?
  // TODO In instead of A
  // TODO this formatting is horrendous
  def boop[In](f: (Boolean, In) => String)(
    implicit eqA: cats.Eq[In]): RE[In, Match[In], _] => String = {
    val showMatch = regex.RegexPrettyPrinter.showMatch(f)
    def go[Out](r: RE[In, Match[In], Out]): (Int, String) =
      RE.fold[In, Match[In], Out, (Int, String)](
        eps = _ => (epsPrecedence, ""),
        fail = () => (failPrecedence, "∅"),
        mappedMatch = (m, _) => (matchPrecedence, showMatch(m)),
        andThen =
          λ[λ[i => (RE[In, Match[In], i => Out], RE[In, Match[In], i])] ~> λ[a => (Int, String)]](
            t =>
              (
                andThenPrecedence,
                parensMaybe(andThenPrecedence, go(t._1), false) + parensMaybe(
                  andThenPrecedence,
                  go(t._2),
                  false))),
        star = λ[λ[i => (RE[In, Match[In], i], Greediness, Out, (Out, i) => Out)] ~> λ[a => (
          Int,
          String)]](t => (starPrecedence, parensMaybe(starPrecedence, go(t._1), true) + "*")),
        mapped = λ[λ[a => (RE[In, Match[In], a], a => Out)] ~> λ[a => (Int, String)]](t => go(t._1)),
        or = alternatives =>
          (
            orPrecedence,
            alternatives.map(r => parensMaybe(orPrecedence, go(r), false)).mkString_("|")),
        void = _ => λ[RE[In, Match[In], ?] ~> λ[a => (Int, String)]](go(_))
      )(r)
    go(_)._2
  }

  val pprintRE: RE[Char, regex.Match[Char], _] => String = {
    boop((inRange, c) => regex.RegexPrettyPrinter.showChar(inRange)(c))
  }
}
