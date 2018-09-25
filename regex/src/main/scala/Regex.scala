package ceedubs.irrec
package regex

import qq.droste.data.prelude._
import qq.droste.data.{Mu, CoattrF}

// TODO ceedubs work around that scala bug where a companion object and type alias have the same name
object Regex {

  def literal[A](value: A): Regex[A] = Mu(CoattrF.pure(Match.Literal(value)))

  def range[A](l: A, r: A): Regex[A] = Mu(CoattrF.pure(Match.Range(l, r)))

  def or[A](l: Regex[A], r: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Plus(l, r)))

  def andThen[A](l: Regex[A], r: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Times(l, r)))

  /**
   * AKA `+` in regular expressions, but I avoided confusion with `Plus` corresponding to "or".
   */
  def oneOrMore[A](value: Regex[A]): Regex[A] = andThen(value, star(value))

  def star[A](value: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Star(value)))

  def wildcard[A]: Regex[A] = Mu(CoattrF.pure(Match.Wildcard))

  /**
   * A match on the empty string (this should always succeed and consume no input).
   */
  def empty[A]: Regex[A] = Mu(CoattrF.roll(KleeneF.One))

  def count[A](n: Int, r: Regex[A]): Regex[A] = (1 to n).foldLeft(empty[A])((acc, _) => andThen(acc, r))

  def stringMatcher[A](r: Regex[Char]): String => Boolean = {
    val n = NFA.runNFA(Glushkov.regexToNFA(r))
    s => n(s.toStream)
  }
}
