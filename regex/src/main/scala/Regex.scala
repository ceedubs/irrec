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
  import Regex._

  def star(greediness: Greediness): Regex[In, M, Chain[Out]] =
    Regex.Star(this, greediness, Chain.empty[Out], (as: Chain[Out], a: Out) => as.append(a))

  def many: Regex[In, M, Chain[Out]] = star(Greediness.Greedy)

  def few: Regex[In, M, Chain[Out]] = star(Greediness.NonGreedy)

  // TODO document
  def oneOrMore(greediness: Greediness): Regex[In, M, NonEmptyChain[Out]] =
    this.map2(this.star(greediness))(NonEmptyChain.fromChainPrepend(_, _))

  def count(n: Int): Regex[In, M, Chain[Out]] =
    Chain.fromSeq(1 to n).traverse(_ => this)

  def repeat(
    minInclusive: Int,
    maxInclusive: Option[Int],
    greediness: Greediness): Regex[In, M, Chain[Out]] = {
    val tail = maxInclusive.fold(star(greediness).some) { max =>
      if (max <= minInclusive) None
      else {
        (0 to (max - minInclusive)).toList.toNel.map { counts =>
          val orderedCounts = greediness match {
            case Greediness.Greedy => counts.reverse
            case Greediness.NonGreedy => counts
          }
          Or(orderedCounts.map(i => count(i)))
        }
      }
    }
    val head = count(minInclusive)
    tail.fold(head)(tail => head.map2(tail)(_ concat _))
  }

  def map[B](f: Out => B): Regex[In, M, B] = FMap(this, f)
}

object Regex {
  // TODO A vs Out
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
    case FMap(r, g) => traverseM(r)(f).map(FMap(_, g))
    case Or(alternatives) => alternatives.traverse(traverseM(_)(f)).map(Or(_))
    case AndThen(l, r) => traverseM(l)(f).map2(traverseM(r)(f))(AndThen(_, _))
    case v @ Void(r) => traverseM[F, In, M, M2, v.Init](r)(f).map(Void(_))
  }

  // TODO change name of Match and use more helpful name than m here
  // TODO think about ordering?
  // TODO can we carry around evidence that Out = Unit for Void?
  def fold[In, M, Out, R](
    eps: Is[Unit, Out] => R,
    fail: () => R,
    elem: (M, In => Option[Out]) => R,
    andThen: λ[i => (Regex[In, M, i => Out], Regex[In, M, i])] ~> λ[a => R],
    star: λ[i => (Regex[In, M, i], Greediness, Out, (Out, i) => Out)] ~> λ[a => R],
    mapped: λ[a => (Regex[In, M, a], a => Out)] ~> λ[a => R],
    or: NonEmptyList[Regex[In, M, Out]] => R,
    void: Is[Unit, Out] => Regex[In, M, ?] ~> λ[a => R]
  )(r: Regex[In, M, Out]): R = r match {
    case AndThen(l, r) => andThen((l, r))
    case Or(alternatives) => or(alternatives)
    case e: Elem[In, M, Out] => elem(e.metadata, e.apply)
    case Star(r, g, z, f) => star((r, g, z, f))
    case FMap(r, f) => mapped((r, f))
    case Eps => eps(Is.refl[Unit])
    case Fail() => fail()
    case Void(r) => void(Is.refl[Unit])(r)
  }

  // TODO move?
  def withMatched[In, M, Out](r: Regex[In, M, Out]): Regex[In, M, (Chain[In], Out)] = r match {
    case AndThen(l, r) =>
      withMatched(l).map2(withMatched(r)) {
        case ((sl, f), (sr, i)) =>
          (sl.concat(sr), f(i))
      }
    case Or(alternatives) => Or(alternatives.map(withMatched))
    case e: Elem[In, M, Out] => Elem(e.metadata, in => e.apply(in).map(o => (Chain.one(in), o)))
    // TODO clean up
    // TODO test
    case rs @ Star(r, g, z, f) =>
      Star[In, M, (Chain[In], rs.Init), (Chain[In], Out)](withMatched(r), g, (Chain.empty[In], z), {
        case ((s0, z), (s1, i)) =>
          (s0 concat s1, f(z, i))
      }): Regex[In, M, (Chain[In], Out)]
    case FMap(r, f) => withMatched(r).map { case (matched, out0) => (matched, f(out0)) }
    case Eps => r.map(o => (Chain.empty, o))
    case Fail() => Fail()
    case v @ Void(r) => withMatched[In, M, v.Init](r).map { case (matched, _) => (matched, ()) }
  }

  // TODO this will probably get created a lot. Reuse a singleton instance?
  implicit def alternativeRegex[In, M]: Alternative[Regex[In, M, ?]] =
    new Alternative[Regex[In, M, ?]] {
      def ap[A, B](ff: Regex[In, M, A => B])(fa: Regex[In, M, A]): Regex[In, M, B] = AndThen(ff, fa)
      def combineK[A](x: Regex[In, M, A], y: Regex[In, M, A]): Regex[In, M, A] = x | y
      def empty[A]: Regex[In, M, A] = Fail()
      def pure[A](x: A): Regex[In, M, A] = FMap[In, M, Unit, A](Eps, _ => x)
      override def map[A, B](fa: Regex[In, M, A])(f: A => B): Regex[In, M, B] = fa.map(f)
      // TODO override void, >*, <*, and as for performance
      //override def void[A](fa: Regex[In,M,A]): Regex[In,M,Unit] = Void(fa)
      // TODO override productL and productR to use Void
    }

  def assignThreadIds[In, M, A](re: Regex[In, M, A]): Regex[In, (ThreadId, M), A] = {
    val freshId: State[ThreadId, ThreadId] = State(id => (ThreadId(id.asInt + 1), id))
    traverseM(re)(m => freshId.map(id => (id, m))).runA(ThreadId(0)).value
  }

  // TODO name
  // TODO could change this to return a natural transformation
  // TODO make private or something?
  // TODO Stream is deprecated in 2.13, right?
  // TODO use Cont/ContT?
  // TODO return a custom type?
  def compileCont[In, M, A, R](
    re: Regex[In, (ThreadId, M), A]): Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]] = {
    type ContOut = Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]]
    Regex.fold[In, (ThreadId, M), A, ContOut](
      eps = ev => _.empty(ev.coerce(())),
      fail = () => _ => Stream.empty,
      // TODO clean up?
      elem = (m, p) =>
        cont =>
          // TODO formatting
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

  // TODO
  def compile[In, M, Out](r: Regex[In, M, Out]): ParseState[In, Out] = {
    val threads =
      Regex
        .compileCont(assignThreadIds(r))
        .apply(Cont.Single((out: Out) => Stream(Thread.Accept[In, Out](out))))
    ParseState.fromThreads(threads)
  }

  // TODO optimize
  // TODO naming/documentation
  // TODO ops class
  def matcher[F[_]: Foldable, In, M, Out](r: Regex[In, M, Out]): F[In] => Boolean = {
    val rc = r.void.compile
    fin => rc.parseOnly(fin).isDefined
  }

  // TODO names
  implicit def toRegexCOps[Out](r: Regex[Char, Match[Char], Out]): RegexCOps[Out] = new RegexCOps(r)

  // TODO name
  implicit def toRegexOps[In, M, Out](r: Regex[In, M, Out]): RegexOps[In, M, Out] = new RegexOps(r)
}
