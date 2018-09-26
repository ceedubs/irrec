package ceedubs.irrec
package regex

import cats.{Foldable, Reducible}
import cats.implicits._
import qq.droste.data.prelude._
import qq.droste.data.{Mu, CoattrF}

// TODO ceedubs work around that scala bug where a companion object and type alias have the same name
object Regex {

  /** alias for [[literal]] */
  def lit[A](value: A): Regex[A] = literal(value)

  def literal[A](value: A): Regex[A] = Mu(CoattrF.pure(Match.Literal(value)))

  def range[A](l: A, r: A): Regex[A] = Mu(CoattrF.pure(Match.Range(l, r)))

  def or[A](l: Regex[A], r: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Plus(l, r)))

  def andThen[A](l: Regex[A], r: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Times(l, r)))

  def oneOf[A](a1: A, as: A*): Regex[A] = as.foldLeft(lit(a1))((acc, a) => or(acc, lit(a)))

  def oneOfR[A](r1: Regex[A], rs: Regex[A]*): Regex[A] = rs.foldLeft(r1)((acc, r) => or(acc, r))

  def oneOfF[F[_], A](values: F[A])(implicit reducibleF: Reducible[F]): Regex[A] =
    reducibleF.reduceLeftTo(values)(lit(_))((acc, a) => or(acc, literal(a)))

  def oneOfFR[F[_], A](values: F[Regex[A]])(implicit reducibleF: Reducible[F]): Regex[A] =
    reducibleF.reduceLeft(values)((acc, r) => or(acc, r))

  /**
   * AKA `+` in regular expressions, but I avoided confusion with `Plus` corresponding to "or".
   */
  def oneOrMore[A](value: Regex[A]): Regex[A] = andThen(value, star(value))

  def star[A](value: Regex[A]): Regex[A] = Mu(CoattrF.roll(KleeneF.Star(value)))

  def wildcard[A]: Regex[A] = Mu(CoattrF.pure(Match.Wildcard))

  def allOfFR[F[_], A](values: F[Regex[A]])(implicit foldableF: Foldable[F]): Regex[A] =
    foldableF.foldLeft(values, empty[A])((acc, a) => andThen(acc, a))

  def allOfF[F[_], A](values: F[A])(implicit foldableF: Foldable[F]): Regex[A] =
    foldableF.foldLeft(values, empty[A])((acc, a) => andThen(acc, literal(a)))

  def allOfR[A](values: Regex[A]*): Regex[A] =
    allOfFR(values.toList)

  def allOf[A](values: A*): Regex[A] =
    allOfF(values.toList)

  def seq[A](values: Seq[A]): Regex[A] =
    values.foldLeft(empty[A])((acc, a) => andThen(acc, literal(a)))

  /**
   * A match on the empty string (this should always succeed and consume no input).
   */
  def empty[A]: Regex[A] = Mu(CoattrF.roll(KleeneF.One))

  /**
   * A regular expression that will never successfully match.
   *
   * This is part of all Kleene algebras but may not be particularly useful in the context of
   * string/character regexes.
   */
  def impossible[A]: Regex[A] = Mu(CoattrF.roll(KleeneF.Zero))

  def count[A](n: Int, r: Regex[A]): Regex[A] = (1 to n).foldLeft(empty[A])((acc, _) => andThen(acc, r))

  def matcher[F[_], A](r: Regex[A])(implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean = {
    NFA.runNFA[F, A, Int](Glushkov.regexToNFA(r))
  }

  def stringMatcher(r: Regex[Char]): String => Boolean = {
    val matcher = r.matcher[List]
    s => matcher(s.toList)
  }
}
