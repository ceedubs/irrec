package ceedubs.irrec
package regex
package applicative

import ceedubs.irrec.regex.Match.MatchSet

import cats.{Order, Reducible, Traverse}
import cats.collections.{Diet, Discrete, Range}
import cats.data.{Chain, NonEmptyList}
import cats.implicits._

// TODO capitalization convention of char vs Regex
// TODO just change RE to Regex and add stuff there?
// TODO what should go in here vs on the class? Both? Separate combinator object?
object Regex {
  // TODO move (and generalize?)
  // TODO what should the different types be?
  // type RegexC[Out] = RE[Char, Match[Char], Out]
  // type RegexG[In, M, Out] =  RE[In, M, Out] // rename?
  // type Regex[In, Out] = RE[In, Match[In], Out]
  // if we did something like this, then Regex._ would be a somewhat reasonable place for these things to go, right?
  // TODO move
  type Regex[In, Out] = RE[In, Match[In], Out]
  type RegexC[A] = RE[Char, Match[Char], A]

  def matching[A: Order](m: Match[A]): Regex[A, A] =
    mapMatch(m, identity)

  def mapMatch[In:Order, Out](m: Match[In], f: In => Out): Regex[In, Out] =
    RE.Match(m, a => if (m.matches(a)) Some(f(a)) else None)

  /** alias for [[literal]] */
  def lit[A: Order](value: A): Regex[A, A] = literal(value)

  def literal[A: Order](value: A): Regex[A, A] = matching(Match.Literal(value))

  def range[A: Order](l: A, r: A): Regex[A, A] = inSet(Diet.fromRange(Range(l, r)))

  def wildcard[A: Order]: Regex[A, A] = matching(Match.Wildcard())

  def or[In, M, Out](l: RE[In, M, Out], r: RE[In, M, Out]): RE[In, M, Out] = l | r

  // TODO tupled or something?
  //def andThen[A](l: Kleene[A], r: Kleene[A]): Kleene[A] = Coattr.roll(KleeneF.Times(l, r))

  def inSet[A: Order](allowed: Diet[A]): Regex[A, A] = matching(MatchSet.allow(allowed))

  def notInSet[A: Order](forbidden: Diet[A]): Regex[A, A] = matching(MatchSet.forbid(forbidden))

  def oneOf[A: Order](a1: A, as: A*): Regex[A, A] =
    RE.Or(NonEmptyList.of(a1, as: _*).map(lit(_)))

  def oneOfR[In, M, Out](r1: RE[In, M, Out], rs: RE[In, M, Out]*): RE[In, M, Out] =
    RE.Or(NonEmptyList.of(r1, rs: _*))

  def oneOfF[F[_], A:Order](values: F[A])(implicit reducibleF: Reducible[F]): Regex[A, A] =
    RE.Or(NonEmptyList.fromReducible(values).map(lit(_)))

  // TODO are some of these even worth having? Can't people just create a NonEmptyList?
  def oneOfFR[F[_], In, M, Out](values: F[RE[In, M, Out]])(implicit reducibleF: Reducible[F]): RE[In, M, Out] =
    RE.Or(NonEmptyList.fromReducible(values))

  def noneOf[A](a1: A, as: A*)(implicit discreteA: Discrete[A], orderA: Order[A]): Regex[A, A] =
    notInSet(NonEmptyList.of(a1, as: _*).foldMap(Diet.one(_)))

  // TODO???
  def allOfFR[F[_], In, M, Out](values: F[RE[In, M, Out]])(implicit traverseF: Traverse[F]): RE[In, M, F[Out]] =
    values.sequence

  def seq[A:Order](values: Seq[A]): Regex[A, Chain[A]] =
    Chain.fromSeq(values).traverse(lit(_))

  // TODO Regex vs RegexG, etc
  def empty[In, M]: RE[In, M, Unit] = RE.Eps

  def fail[A]: RE[Any, Nothing, A] = RE.Fail()
}
