package ceedubs.irrec
package regex

import cats.{~>, Alternative, Applicative, Foldable}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, State}
import cats.evidence.Is
import cats.implicits._

/**
 * A generalized representation of a regular expression.
 *
 * This code was ported (with minor modifications) from
 * https://hackage.haskell.org/package/regex-applicative
 *
 * @tparam In the type of each element in the input. For a traditional regular expression, this
 * would be `Char`.
 *
 * @tparam M metadata associated with each [[ceedubs.irrec.regex.Regex.Elem]] instance. This could
 * be `String` to attach a name to elems. More commonly, it is something like
 * [[ceedubs.irrec.regex.Match]] with a `Char` type parameter.
 *
 * @tparam Out the output produced by a regular expression if the input matches.
 *
 * Additional useful methods are added via syntax enrichment with [[ceedubs.irrec.regex.RegexOps]]
 * and [[ceedubs.irrec.regex.RegexCOps]].
 */
sealed abstract class Regex[-In, +M, Out] extends Serializable {
  def star(greediness: Greediness): Regex[In, M, Chain[Out]] =
    combinator.star(this, greediness)

  def many: Regex[In, M, Chain[Out]] = combinator.many(this)

  def few: Regex[In, M, Chain[Out]] = combinator.few(this)

  def oneOrMore(greediness: Greediness): Regex[In, M, NonEmptyChain[Out]] =
    combinator.oneOrMore(this, greediness)

  def count(n: Int): Regex[In, M, Chain[Out]] = combinator.count(n, this)

  def repeat(
    minInclusive: Int,
    maxInclusive: Option[Int],
    greediness: Greediness): Regex[In, M, Chain[Out]] =
    combinator.repeat(this, minInclusive, maxInclusive, greediness)

  def map[Out2](f: Out => Out2): Regex[In, M, Out2] = combinator.map(this)(f)
}

object Regex {
  case object Eps extends Regex[Any, Nothing, Unit]
  final case class Fail[A]() extends Regex[Any, Nothing, A]
  // TODO should this actually have both? Instead could just rely on doing a `map` and changing `M`.
  // But that assumes that the conversion will be the same for all M
  // Also that's probably going to mess with type inference
  abstract class Elem[-In, +M, Out] extends Regex[In, M, Out] {
    def metadata: M
    def apply(in: In): Option[Out]
  }

  object Elem {
    def apply[In, M, Out](m: M, f: In => Option[Out]): Elem[In, M, Out] = new Elem[In, M, Out] {
      def metadata: M = m
      def apply(in: In): Option[Out] = f(in)
    }
  }

  final case class AndThen[-In, +M, I, Out](l: Regex[In, M, I => Out], r: Regex[In, M, I])
      extends Regex[In, M, Out] {
    type Init = I
  }
  // TODO use a lazy structure like NonEmptyStream?
  final case class Or[-In, +M, Out](alternatives: NonEmptyList[Regex[In, M, Out]])
      extends Regex[In, M, Out]

  final case class FMap[-In, +M, I, Out](r: Regex[In, M, I], f: I => Out)
      extends Regex[In, M, Out] {
    type Init = I
  }

  final case class Star[-In, +M, I, Out](
    r: Regex[In, M, I],
    greediness: Greediness,
    z: Out,
    fold: (Out, I) => Out)
      extends Regex[In, M, Out] {
    type Init = I
  }

  final case class Repeat[-In, +M, I, Out](
    r: Regex[In, M, I],
    minInclusive: Int,
    maxInclusive: Option[Int],
    greediness: Greediness,
    z: Out,
    fold: (Out, I) => Out)
      extends Regex[In, M, Out] {
    type Init = I

    def expand: Regex[In, M, Out] = {
      val tail = maxInclusive.fold(combinator.star(r, greediness).some) { max =>
        if (max <= minInclusive) None
        else {
          (0 to (max - minInclusive)).toList.toNel.map { counts =>
            val orderedCounts = greediness match {
              // TODO reversed?
              case Greediness.Greedy => counts.reverse
              case Greediness.NonGreedy => counts
            }
            Regex.Or(orderedCounts.map(i => expandedCount(i, r)))
          }
        }
      }
      val head = expandedCount(minInclusive, r)
      tail
        .fold(head)(tail => head.map2(tail)(_ concat _))
        .map(_.foldLeft(z)(fold))
    }
  }

  // TODO efficiently handle with NFA
  final case class Void[-In, +M, I](r: Regex[In, M, I]) extends Regex[In, M, Unit] {
    type Init = I
  }

  // TODO document
  def traverseM[F[_], In, M, M2, Out](re: Regex[In, M, Out])(f: M => F[M2])(
    implicit F: Applicative[F]): F[Regex[In, M2, Out]] = re match {
    case e: Elem[In, M, Out] => f(e.metadata).map(Elem(_, e.apply))
    case Eps => F.pure(Eps)
    case x @ Fail() => F.pure(x)
    case Star(r, g, z, fold) => traverseM(r)(f).map(Star(_, g, z, fold))
    case Repeat(r, min, max, g, z, fold) => traverseM(r)(f).map(Repeat(_, min, max, g, z, fold))
    case FMap(r, g) => traverseM(r)(f).map(FMap(_, g))
    case Or(alternatives) => alternatives.traverse(traverseM(_)(f)).map(Or(_))
    case AndThen(l, r) => traverseM(l)(f).map2(traverseM(r)(f))(AndThen(_, _))
    case v @ Void(r) => traverseM[F, In, M, M2, v.Init](r)(f).map(Void(_))
  }

  def fold[In, M, Out, R](
    eps: Is[Unit, Out] => R,
    fail: () => R,
    elem: (M, In => Option[Out]) => R,
    andThen: λ[i => (Regex[In, M, i => Out], Regex[In, M, i])] ~> λ[a => R],
    star: λ[i => (Regex[In, M, i], Greediness, Out, (Out, i) => Out)] ~> λ[a => R],
    repeat: λ[i => (Regex[In, M, i], Int, Option[Int], Greediness, Out, (Out, i) => Out)] ~> λ[
      a => R],
    mapped: λ[a => (Regex[In, M, a], a => Out)] ~> λ[a => R],
    or: NonEmptyList[Regex[In, M, Out]] => R,
    void: Is[Unit, Out] => Regex[In, M, ?] ~> λ[a => R]
  )(r: Regex[In, M, Out]): R = r match {
    case AndThen(l, r) => andThen((l, r))
    case Or(alternatives) => or(alternatives)
    case e: Elem[In, M, Out] => elem(e.metadata, e.apply)
    case Star(r, g, z, f) => star((r, g, z, f))
    case Repeat(r, min, max, g, z, f) => repeat((r, min, max, g, z, f))
    case FMap(r, f) => mapped((r, f))
    case Eps => eps(Is.refl[Unit])
    case Fail() => fail()
    case Void(r) => void(Is.refl[Unit])(r)
  }

  // TODO this will probably get created a lot. Reuse a singleton instance?
  implicit def alternativeRegex[In, M]: Alternative[Regex[In, M, ?]] =
    new Alternative[Regex[In, M, ?]] {
      def ap[A, B](ff: Regex[In, M, A => B])(fa: Regex[In, M, A]): Regex[In, M, B] = AndThen(ff, fa)
      def combineK[A](x: Regex[In, M, A], y: Regex[In, M, A]): Regex[In, M, A] = x | y
      def empty[A]: Regex[In, M, A] = Fail()
      def pure[A](x: A): Regex[In, M, A] = FMap[In, M, Unit, A](Eps, _ => x)
      override def map[A, B](fa: Regex[In, M, A])(f: A => B): Regex[In, M, B] = fa.map(f)
      override def void[A](fa: Regex[In, M, A]): Regex[In, M, Unit] = fa match {
        case v @ Regex.Void(_) => v
        case r => Regex.Void(r)
      }
      // when a result is ignored, using `void` to delegate to an NFA is more efficient
      override def productL[A, B](fa: Regex[In, M, A])(fb: Regex[In, M, B]): Regex[In, M, A] =
        super.productL(fa)(void(fb))
      override def productR[A, B](fa: Regex[In, M, A])(fb: Regex[In, M, B]): Regex[In, M, B] =
        super.productR(void(fa))(fb)
    }

  def assignThreadIds[In, M, A](re: Regex[In, M, A]): Regex[In, (ThreadId, M), A] = {
    val freshId: State[ThreadId, ThreadId] = State(id => (ThreadId(id.asInt + 1), id))
    traverseM(re)(m => freshId.map(id => (id, m))).runA(ThreadId(0)).value
  }

  // TODO could change this to return a natural transformation
  // TODO make private or something?
  // TODO Stream is deprecated in 2.13, right?
  // TODO use Cont/ContT?
  // TODO return a custom type?
  private def compileCont[In, M, A, R](
    re: Regex[In, (ThreadId, M), A]): Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]] = {
    type ContOut = Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]]
    Regex.fold[In, (ThreadId, M), A, ContOut](
      eps = ev => _.empty(ev.coerce(())),
      fail = () => _ => Stream.empty,
      elem = (m, p) =>
        cont =>
          Thread
            .Cont[In, R](m._1, in => p(in).fold(Stream.empty[Thread[In, R]])(cont.nonEmpty(_))) #:: Stream.empty,
      andThen = new (λ[i => (Regex[In, (ThreadId, M), i => A], Regex[In, (ThreadId, M), i])] ~> λ[
        a => ContOut]) {
        def apply[i](
          t: (Regex[In, (ThreadId, M), i => A], Regex[In, (ThreadId, M), i])): ContOut = {
          val lc = compileCont[In, M, i => A, R](t._1)
          val rc = compileCont[In, M, i, R](t._2)
          _ match {
            case Cont.Single(f) => lc(Cont.Single(lVal => rc(Cont.Single(f compose lVal))))
            case Cont.Choice(whenEmpty, whenNonEmpty) =>
              lc(
                Cont.Choice(
                  whenEmpty = lVal =>
                    rc(Cont.Choice(whenEmpty compose lVal, whenNonEmpty compose lVal)),
                  whenNonEmpty = lVal => rc(Cont.Single(whenNonEmpty compose lVal))
                ))
          }
        }
      },
      star =
        new (λ[i => (Regex[In, (ThreadId, M), i], Greediness, A, (A, i) => A)] ~> λ[a => ContOut]) {
          def apply[i](t: (Regex[In, (ThreadId, M), i], Greediness, A, (A, i) => A)): ContOut = {
            val (r, g, z, f) = t
            val rc = compileCont[In, M, i, R](r)
            def threads(z: A, cont: Cont[A => Stream[Thread[In, R]]]): Stream[Thread[In, R]] = {
              def stop = cont.empty(z)
              // TODO think more about laziness
              def go =
                rc(Cont.Choice(whenEmpty = _ => Stream.empty, whenNonEmpty = { v =>
                  threads(f(z, v), Cont.Single(cont.nonEmpty))
                }))
              g match {
                case Greediness.Greedy => go #::: stop
                case Greediness.NonGreedy => stop #::: go
              }
            }
            threads(z, _)
          }
        },
      repeat = new (λ[i => (
        Regex[In, (ThreadId, M), i],
        Int,
        Option[Int],
        Greediness,
        A,
        (A, i) => A)] ~> λ[a => ContOut]) {
        def apply[i](
          t: (Regex[In, (ThreadId, M), i], Int, Option[Int], Greediness, A, (A, i) => A)): ContOut =
          sys.error(
            "compileCont called with a Repeat instance that hadn't been expanded. This should never happen.")
      },
      mapped = new (λ[a => (Regex[In, (ThreadId, M), a], a => A)] ~> λ[a => ContOut]) {
        def apply[i](t: (Regex[In, (ThreadId, M), i], i => A)): ContOut = {
          val rc = compileCont[In, M, i, R](t._1)
          cont => rc(cont.map(_ compose t._2))
        }
      },
      or = alternatives => {
        val alternativesC = alternatives.map(compileCont[In, M, A, R](_)).toList.toStream
        cont => alternativesC.flatMap(_.apply(cont))
      },
      void = ev =>
        λ[Regex[In, (ThreadId, M), ?] ~> λ[a => ContOut]](r =>
          compileCont(r.map(_ => ev.coerce(()))))
    )(re)
  }

  private def expandRepeat[In, M]: Regex[In, M, ?] ~> Regex[In, M, ?] =
    new (Regex[In, M, ?] ~> Regex[In, M, ?]) {
      def apply[A](r: Regex[In, M, A]): Regex[In, M, A] = r match {
        case r: Repeat[_, _, _, _] => r.expand
        case r => r
      }
    }

  def compile[In, M, Out](r: Regex[In, M, Out]): ParseState[In, Out] = {
    val threads =
      Regex
        .compileCont(assignThreadIds(transformRecursive(expandRepeat)(r)))
        .apply(Cont.Single((out: Out) => Stream(Thread.Accept[In, Out](out))))
    ParseState.fromThreads(threads)
  }

  def transformRecursive[In, M](
    f: Regex[In, M, ?] ~> Regex[In, M, ?]): Regex[In, M, ?] ~> Regex[In, M, ?] =
    new (Regex[In, M, ?] ~> Regex[In, M, ?]) {
      def apply[A](fa: Regex[In, M, A]): Regex[In, M, A] = fa match {
        case Eps => f(Eps)
        case x @ Fail() => f(x)
        case x: Elem[_, _, _] => f(x)
        case AndThen(l, r) => f(AndThen(apply(l), apply(r)))
        case Or(alternatives) => f(Or(alternatives.map(apply(_))))
        case FMap(r, g) => f(FMap(apply(r), g))
        case Star(r, greediness, z, fold) => f(Star(apply(r), greediness, z, fold))
        case Repeat(r, minInclusive, maxInclusive, greediness, z, fold) =>
          f(Repeat(apply(r), minInclusive, maxInclusive, greediness, z, fold))
        case Void(r) => f(Void(apply(r)))
      }
    }

  private def expandedCount[In, M, Out](n: Int, r: Regex[In, M, Out]): Regex[In, M, Chain[Out]] =
    Chain.fromSeq(1 to n).traverse(_ => r)

  // TODO optimize
  // TODO naming/documentation
  def matcher[F[_]: Foldable, In, M, Out](r: Regex[In, M, Out]): F[In] => Boolean = {
    val rc = r.void.compile
    fin => rc.parseOnly(fin).isDefined
  }

  implicit def toRegexCOps[Out](r: Regex[Char, Match[Char], Out]): RegexCOps[Out] = new RegexCOps(r)

  implicit def toRegexOps[In, M, Out](r: Regex[In, M, Out]): RegexOps[In, M, Out] = new RegexOps(r)
}
