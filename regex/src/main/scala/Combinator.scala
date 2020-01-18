package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Match.MatchSet

import cats.{Order, Reducible, Traverse}
import cats.collections.{Diet, Discrete, Range}
import cats.data.{Chain, NonEmptyList}
import cats.implicits._

// TODO star-like method that allows z/fold params
object combinator {
  def matching[A: Order](m: Match[A]): RegexM[A, A] =
    mapMatch(m, identity)

  def mapMatch[In: Order, Out](m: Match[In], f: In => Out): RegexM[In, Out] =
    Regex.Elem(m, a => if (m.matches(a)) Some(f(a)) else None)

  /** alias for [[literal]] */
  def lit[A: Order](value: A): RegexM[A, A] = literal(value)

  def literal[A: Order](value: A): RegexM[A, A] = matching(Match.Literal(value))

  def range[A: Order](l: A, r: A): RegexM[A, A] = inSet(Diet.fromRange(Range(l, r)))

  def wildcard[A: Order]: RegexM[A, A] = matching(Match.Wildcard())

  // TODO add in optimizations during constructions like this or have a separate method to optimize?
  def or[In, M, Out](l: Regex[In, M, Out], r: Regex[In, M, Out]): Regex[In, M, Out] = (l, r) match {
    case (Regex.Or(xs), Regex.Or(ys)) => Regex.Or(xs ::: ys)
    case (_, Regex.Fail()) => l
    case (Regex.Fail(), _) => r
    case (Regex.Or(xs), _) => Regex.Or(r :: xs)
    case (_, Regex.Or(ys)) => Regex.Or(l :: ys)
    case _ => Regex.Or(NonEmptyList(l, r :: Nil))
  }

  // TODO test
  def either[In, M, Out1, Out2](
    l: Regex[In, M, Out1],
    r: Regex[In, M, Out2]): Regex[In, M, Either[Out1, Out2]] =
    l.map(Either.left[Out1, Out2](_)) | r.map(Either.right[Out1, Out2](_))

  def inSet[A: Order](allowed: Diet[A]): RegexM[A, A] = matching(MatchSet.allow(allowed))

  def notInSet[A: Order](forbidden: Diet[A]): RegexM[A, A] = matching(MatchSet.forbid(forbidden))

  def oneOf[A: Order](a1: A, as: A*): RegexM[A, A] =
    Regex.Or(NonEmptyList.of(a1, as: _*).map(lit(_)))

  def oneOfR[In, M, Out](r1: Regex[In, M, Out], rs: Regex[In, M, Out]*): Regex[In, M, Out] =
    Regex.Or(NonEmptyList.of(r1, rs: _*))

  def oneOfF[F[_], A: Order](values: F[A])(implicit reducibleF: Reducible[F]): RegexM[A, A] =
    Regex.Or(NonEmptyList.fromReducible(values).map(lit(_)))

  // TODO are some of these even worth having? Can't people just create a NonEmptyList?
  def oneOfFR[F[_], In, M, Out](values: F[Regex[In, M, Out]])(
    implicit reducibleF: Reducible[F]): Regex[In, M, Out] =
    Regex.Or(NonEmptyList.fromReducible(values))

  def noneOf[A](a1: A, as: A*)(implicit discreteA: Discrete[A], orderA: Order[A]): RegexM[A, A] =
    notInSet(NonEmptyList.of(a1, as: _*).foldMap(Diet.one(_)))

  // TODO???
  // TODO a lot of these aren't specific to Match are they?
  def allOfFR[F[_], In, M, Out](values: F[Regex[In, M, Out]])(
    implicit traverseF: Traverse[F]): Regex[In, M, F[Out]] =
    values.sequence

  def seq[A: Order](values: Seq[A]): RegexM[A, Chain[A]] =
    Chain.fromSeq(values).traverse(lit(_))

  def allOf[A: Order](values: A*): RegexM[A, Chain[A]] =
    Chain.fromSeq(values).traverse(lit(_))

  def allOfF[F[_]: Traverse, A: Order](values: F[A]): RegexM[A, F[A]] =
    values.traverse(lit(_))

  def allOfR[In, M, Out](values: Regex[In, M, Out]*): Regex[In, M, Chain[Out]] =
    allOfFR(Chain.fromSeq(values))

  def empty[In, M]: Regex[In, M, Unit] = Regex.Eps

  def fail[A]: Regex[Any, Nothing, A] = Regex.Fail()
}
