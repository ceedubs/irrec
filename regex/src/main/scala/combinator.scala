package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Match.MatchSet

import cats.{Order, Reducible, Traverse}
import cats.collections.{Diet, Discrete, Range}
import cats.data.{Chain, NonEmptyChain, NonEmptyList}
import cats.implicits._

object combinator {
  import Regex._

  /**
   * A match on a single input element.
   */
  def elem[In, M, Out](f: In => Option[Out], metadata: M = "elem"): Regex[In, M, Out] =
    Regex.Elem(metadata, f)

  /**
   * A predicate to run on a single input element.
   *
   * @param f the predicate - should return true if the element matches and false otherwise
   */
  def pred[In, M, Out](f: In => Boolean, metadata: M = "pred"): Regex[In, M, In] =
    Regex.Elem(metadata, in => if (f(in)) Some(in) else None)

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
  def or[In, M, Out](l: Regex[In, M, Out], r: Regex[In, M, Out]): Regex[In, M, Out] =
    (l, r) match {
      case (Regex.Or(xs), Regex.Or(ys)) => Regex.Or(xs ::: ys)
      case (_, Regex.Fail()) => l
      case (Regex.Fail(), _) => r
      case (Regex.Or(xs), _) => Regex.Or(xs :+ r)
      case (_, Regex.Or(ys)) => Regex.Or(l :: ys)
      case _ => Regex.Or(NonEmptyList(l, r :: Nil))
    }

  def quantifyFold[In, M, Out1, Out2](r: Regex[In, M, Out1], q: Quantifier, z: Out2)(
    fold: (Out2, Out1) => Out2): Regex[In, M, Out2] =
    Regex.Repeat(r, q, z, fold)

  def optional[In, M, Out](
    r: Regex[In, M, Out],
    greediness: Greediness): Regex[In, M, Option[Out]] =
    quantifyFold(r, Quantifier.Optional(greediness), none[Out])((_, o) => Some(o))

  def either[In, M, Out1, Out2](
    l: Regex[In, M, Out1],
    r: Regex[In, M, Out2]): Regex[In, M, Either[Out1, Out2]] =
    l.map(Either.left[Out1, Out2](_)) | r.map(Either.right[Out1, Out2](_))

  def starFold[In, M, Out1, Out2](r: Regex[In, M, Out1], g: Greediness, z: Out2)(
    fold: (Out2, Out1) => Out2): Regex[In, M, Out2] =
    Regex.Star(r, g, z, fold)

  def star[In, M, Out](r: Regex[In, M, Out], g: Greediness): Regex[In, M, Chain[Out]] =
    starFold(r, g, Chain.empty[Out])(_ append _)

  def many[In, M, Out](r: Regex[In, M, Out]): Regex[In, M, Chain[Out]] =
    star(r, Greediness.Greedy)

  def few[In, M, Out](r: Regex[In, M, Out]): Regex[In, M, Chain[Out]] =
    star(r, Greediness.NonGreedy)

  def count[In, M, Out](n: Int, r: Regex[In, M, Out]): Regex[In, M, Chain[Out]] =
    quantifyFold(r, Quantifier.Exact(n), Chain.empty[Out])(_ append _)

  def repeat[In, M, Out](
    r: Regex[In, M, Out],
    minInclusive: Int,
    maxInclusive: Option[Int],
    greediness: Greediness): Regex[In, M, Chain[Out]] =
    quantifyFold(r, Quantifier.Range(minInclusive, maxInclusive, greediness), Chain.empty[Out])(
      _.append(_))

  def oneOrMore[In, M, Out](
    r: Regex[In, M, Out],
    greediness: Greediness): Regex[In, M, NonEmptyChain[Out]] =
    r.map2(r.star(greediness))(NonEmptyChain.fromChainPrepend(_, _))

  def map[In, M, Out, Out2](r: Regex[In, M, Out])(f: Out => Out2): Regex[In, M, Out2] =
    Regex.FMap(r, f)

  def inSet[A: Order](allowed: Diet[A]): RegexM[A, A] = matching(MatchSet.allow(allowed))

  def notInSet[A: Order](forbidden: Diet[A]): RegexM[A, A] = matching(MatchSet.forbid(forbidden))

  def oneOf[A: Order](a1: A, as: A*): RegexM[A, A] =
    Regex.Or(NonEmptyList.of(a1, as: _*).map(lit(_)))

  def oneOfR[In, M, Out](r1: Regex[In, M, Out], rs: Regex[In, M, Out]*): Regex[In, M, Out] =
    Regex.Or(NonEmptyList.of(r1, rs: _*))

  def oneOfF[F[_], A: Order](values: F[A])(implicit reducibleF: Reducible[F]): RegexM[A, A] =
    Regex.Or(NonEmptyList.fromReducible(values).map(lit(_)))

  def oneOfFR[F[_], In, M, Out](values: F[Regex[In, M, Out]])(implicit
    reducibleF: Reducible[F]): Regex[In, M, Out] =
    Regex.Or(NonEmptyList.fromReducible(values))

  def noneOf[A](a1: A, as: A*)(implicit discreteA: Discrete[A], orderA: Order[A]): RegexM[A, A] =
    notInSet(NonEmptyList.of(a1, as: _*).foldMap(Diet.one(_)))

  def allOfFR[F[_], In, M, Out](values: F[Regex[In, M, Out]])(implicit
    traverseF: Traverse[F]): Regex[In, M, F[Out]] =
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

  def withMatched[In, M, Out](r: Regex[In, M, Out]): Regex[In, M, (Chain[In], Out)] =
    r match {
      case AndThen(l, r) =>
        withMatched(l).map2(withMatched(r)) { case ((sl, f), (sr, i)) =>
          (sl.concat(sr), f(i))
        }
      case Or(alternatives) => Or(alternatives.map(withMatched))
      case e: Elem[In, M, Out] => Elem(e.metadata, in => e.apply(in).map(o => (Chain.one(in), o)))
      case rs @ Star(r, g, z, f) =>
        Star[In, M, (Chain[In], rs.Init), (Chain[In], Out)](
          withMatched(r),
          g,
          (Chain.empty[In], z),
          { case ((s0, z), (s1, i)) =>
            (s0 concat s1, f(z, i))
          }): Regex[In, M, (Chain[In], Out)]
      case rs @ Repeat(r, q, z, f) =>
        Repeat[In, M, (Chain[In], rs.Init), (Chain[In], Out)](
          withMatched(r),
          q,
          (Chain.empty[In], z),
          { case ((s0, z), (s1, i)) =>
            (s0 concat s1, f(z, i))
          }): Regex[In, M, (Chain[In], Out)]
      case FMap(r, f) => withMatched(r).map { case (matched, out0) => (matched, f(out0)) }
      case MapFilter(r, f) =>
        withMatched(r).mapFilter { case (matched, o1) => f(o1).map(o => (matched, o)) }
      case Eps => r.map(o => (Chain.empty, o))
      case Fail() => Fail()
      case v @ Void(r) => withMatched[In, M, v.Init](r).map { case (matched, _) => (matched, ()) }
    }

  def matched[In, M, Out](r: Regex[In, M, Out]): Regex[In, M, Chain[In]] = withMatched(r).map(_._1)

  def andThen[In, M, Out1, Out2](
    rOut: Regex[In, M, Out1 => Out2],
    rIn: Regex[In, M, Out1]): Regex[In, M, Out2] = AndThen(rOut, rIn)
}
