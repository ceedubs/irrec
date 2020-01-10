package ceedubs.irrec
package regex
package applicative

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
  def boop[A](f: (Boolean, A) => String)(implicit eqA: cats.Eq[A]): RE[_, Match[A], _] => String = {
    val showMatch = regex.RegexPrettyPrinter.showMatch(f)
    def go(r: RE[_, Match[A], _]): (Int, String) =
      r match {
        case RE.Or(alternatives) => (orPrecedence, alternatives.map(r => parensMaybe(orPrecedence, go(r), false)).mkString_("|"))
        case RE.Fail() => (failPrecedence, "∅")
        // TODO handle greediness
        case RE.Star(r, _, _, _) => (starPrecedence, parensMaybe(starPrecedence, go(r), true) + "*")
        case RE.Eps => (epsPrecedence, "")
        case RE.Match(m, _) => (matchPrecedence, showMatch(m))
        case RE.FMap(r, _) => go(r)
        // TODO is this right?
        case RE.AndThen(l, r) => (andThenPrecedence, parensMaybe(andThenPrecedence, go(l), false) + parensMaybe(andThenPrecedence, go(r), false))
        // This is gross but _should_ be safe.
        // I seem to be running into https://github.com/scala/bug/issues/10292
        // TODO can I set up a fold method to avoid this issue?
        case v => go(v.asInstanceOf[RE.Void[_, Match[A], _]].r)
      }
    go(_)._2
  }

  val pprintRE: RE[Char, regex.Match[Char], _] => String = {
    boop((inRange, c) => regex.RegexPrettyPrinter.showChar(inRange)(c))
  }
}
