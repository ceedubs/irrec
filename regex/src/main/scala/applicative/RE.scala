package ceedubs.irrec
package regex
package applicative

// TODO
import ceedubs.irrec.regex.{Regex => RegexOld}

import cats.{~>, Alternative, Applicative, Foldable}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, State}
import cats.evidence.Is
import cats.implicits._
import higherkindness.droste.data.Coattr

// This code was ported (with minor modifications) from https://hackage.haskell.org/package/regex-applicative
// TODO document type parameters (especially M)
sealed abstract class RE[-In, +M, Out] extends Serializable {
  import RE._

  // TODO add ops on to avoid variance shenanigans?
  // TODO add in optimizations during constructions like this or have a separate method to optimize?
  def |[In2 <: In, M2 >: M](o: RE[In2, M2, Out]): RE[In2, M2, Out] = (this, o) match {
    case (Or(xs), Or(ys)) => Or(xs ::: ys)
    case (_, Fail()) => this
    case (Fail(), _) => o
    case (Or(xs), _) => Or(o :: xs)
    case (_, Or(ys)) => Or(this :: ys)
    case _ => Or(NonEmptyList(this, o :: Nil))
  }

  def star(greediness: Greediness): RE[In, M, Chain[Out]] =
    RE.Star(this, greediness, Chain.empty[Out], (as: Chain[Out], a: Out) => as.append(a))

  def many: RE[In, M, Chain[Out]] = star(Greediness.Greedy)

  def few: RE[In, M, Chain[Out]] = star(Greediness.NonGreedy)

  // TODO document
  def oneOrMore(greediness: Greediness): RE[In, M, NonEmptyChain[Out]] =
    this.map2(this.star(greediness))(NonEmptyChain.fromChainPrepend(_, _))

  def count(n: Int): RE[In, M, Chain[Out]] =
    Chain.fromSeq(1 to n).traverse(_ => this)

  // TODO is this handling greediness right? Test this.
  def repeat(
    minInclusive: Int,
    maxInclusive: Option[Int],
    greediness: Greediness): RE[In, M, Chain[Out]] = {
    val tail = maxInclusive.fold(star(greediness).some) { max =>
      if (max <= minInclusive) None
      else {
        (0 to (max - minInclusive)).toList.toNel.map { counts =>
          val orderedCounts = greediness match {
            case Greediness.Greedy => counts.reverse
            case Greediness.NonGreedy => counts
          }
          // TODO this is a mess
          Or(orderedCounts.map(i => count(i)))
        }
      }
    }
    val head = count(minInclusive)
    tail.fold(head)(tail => head.map2(tail)(_ concat _))
  }

  def optional[In2 <: In, M2 >: M]: RE[In2, M2, Option[Out]] =
    this.map[Option[Out]](Some(_)) | none[Out].pure[RE[In2, M2, ?]]

  def map[B](f: Out => B): RE[In, M, B] = FMap(this, f)

  def compile[In2 <: In]: ParseState[In2, Out] = RE.compile(this)

}

object RE {
  // TODO A vs Out
  case object Eps extends RE[Any, Nothing, Unit]
  final case class Fail[A]() extends RE[Any, Nothing, A]
  // TODO should this actually have both? Instead could just rely on doing a `map` and changing `M`.
  // But that assumes that the conversion will be the same for all M
  // Also that's probably going to mess with type inference
  abstract class Elem[-In, +M, Out] extends RE[In, M, Out] {
    def metadata: M
    def apply(in: In): Option[Out]
  }

  object Elem {
    def apply[In, M, Out](m: M, f: In => Option[Out]): Elem[In, M, Out] = new Elem[In, M, Out]{
      def metadata: M = m
      def apply(in: In): Option[Out] = f(in)
    }
  }

  final case class AndThen[-In, +M, I, Out](l: RE[In, M, I =>Out], r: RE[In, M, I])
      extends RE[In, M, Out] {
    type Init = I
  }
  // TODO use a lazy structure like NonEmptyStream?
  final case class Or[-In, +M, Out](alternatives: NonEmptyList[RE[In, M, Out]]) extends RE[In, M, Out]

  final case class FMap[-In, +M, I, Out](r: RE[In, M, I], f: I => Out) extends RE[In, M, Out] {
    type Init = I
  }

  final case class Star[-In, +M, I, Out](
    r: RE[In, M, I],
    greediness: Greediness,
    z: Out,
    fold: (Out, I) => Out)
      extends RE[In, M, Out] {
    type Init = I
  }
  // TODO efficiently handle with NFA
  final case class Void[-In, +M, I](r: RE[In, M, I]) extends RE[In, M, Unit] {
    type Init = I
  }

  // TODO document
  def traverseM[F[_], In, M, M2, Out](re: RE[In, M, Out])(f: M => F[M2])(
    implicit F: Applicative[F]): F[RE[In, M2, Out]] = re match {
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
    mappedMatch: (M, In => Option[Out]) => R,
    andThen: λ[i => (RE[In, M, i => Out], RE[In, M, i])] ~> λ[a => R],
    star: λ[i => (RE[In, M, i], Greediness, Out, (Out, i) => Out)] ~> λ[a => R],
    mapped: λ[a => (RE[In, M, a], a => Out)] ~> λ[a => R],
    or: NonEmptyList[RE[In, M, Out]] => R,
    void: Is[Unit, Out] => RE[In, M, ?] ~> λ[a => R]
    )(r: RE[In, M, Out]): R = r match {
    case AndThen(l, r) => andThen((l, r))
    case Or(alternatives) => or(alternatives)
    case e: Elem[In, M, Out] => mappedMatch(e.metadata, e.apply)
    case Star(r, g, z, f) => star((r, g, z, f))
    case FMap(r, f) => mapped((r, f))
    case Eps => eps(Is.refl[Unit])
    case Fail() => fail()
    case Void(r) => void(Is.refl[Unit])(r)
  }

  implicit def alternativeRE[In, M]: Alternative[RE[In, M, ?]] = new Alternative[RE[In, M, ?]] {
    def ap[A, B](ff: RE[In, M, A => B])(fa: RE[In, M, A]): RE[In, M, B] = AndThen(ff, fa)
    def combineK[A](x: RE[In, M, A], y: RE[In, M, A]): RE[In, M, A] = x | y
    def empty[A]: RE[In, M, A] = Fail()
    def pure[A](x: A): RE[In, M, A] = FMap[In, M, Unit, A](Eps, _ => x)
    override def map[A, B](fa: RE[In, M, A])(f: A => B): RE[In, M, B] = fa.map(f)
    // TODO
    //override def void[A](fa: RE[In,M,A]): RE[In,M,Unit] = Void(fa)
    // TODO override productL and productR to use Void
  }

  def assignThreadIds[In, M, A](re: RE[In, M, A]): RE[In, (ThreadId, M), A] = {
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
    re: RE[In, (ThreadId, M), A]): Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]] = {
    type ContOut = Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]]
    RE.fold[In, (ThreadId, M), A, ContOut](
      eps = ev => _.empty(ev.coerce(())),
      fail = () => _ => Stream.empty,
      // TODO clean up?
      mappedMatch = (m, p) =>
        cont =>
          // TODO formatting
          Thread
            .Cont[In, R](m._1, in => p(in).fold(Stream.empty[Thread[In, R]])(cont.nonEmpty(_))) #:: Stream.empty,
      andThen = new (λ[i => (RE[In, (ThreadId, M), i => A], RE[In, (ThreadId, M), i])] ~> λ[a => ContOut]){
        def apply[i](t: (RE[In, (ThreadId, M), i => A], RE[In, (ThreadId, M), i])): ContOut = {
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
      star =  new (λ[i => (RE[In, (ThreadId, M), i], Greediness, A, (A, i) => A)] ~> λ[a => ContOut]){
        def apply[i](t: (RE[In, (ThreadId, M), i], Greediness, A, (A, i) => A)): ContOut = {
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
      mapped = new (λ[a => (RE[In, (ThreadId, M), a], a => A)] ~> λ[a => ContOut]){
        def apply[i](t: (RE[In, (ThreadId, M), i], i => A)): ContOut = {
          val rc = compileCont[In, M, i, R](t._1)
          cont => rc(cont.map(_ compose t._2))
        }
      },
      or = alternatives => {
        val alternativesC = alternatives.map(compileCont[In, M, A, R](_)).toList.toStream
        cont => alternativesC.flatMap(_.apply(cont))
      },
      void = ev => λ[RE[In, (ThreadId, M), ?] ~> λ[a => ContOut]](r => compileCont(r.map(_ => ev.coerce(()))))
      )(re)
  }

  // TODO
  def compile[In, M, Out](r: RE[In, M, Out]): ParseState[In, Out] = {
    val threads =
      RE.compileCont(assignThreadIds(r))
        .apply(Cont.Single((out: Out) => Stream(Thread.Accept[In, Out](out))))
    ParseState.fromThreads(threads)
  }

  // TODO name?
  // TODO should this exist?
  def toKleene[M](r: RE[_, M, _]): Kleene[M] = r match {
    case Eps => RegexOld.empty
    case Fail() => RegexOld.impossible
    case FMap(r, _) => toKleene(r)
    case Star(r, _, _, _) => toKleene(r).star
    case AndThen(l, r) => toKleene(l) * toKleene(r)
    case e: Elem[_, M, _] => Coattr.pure(e.metadata)
    case Or(alternatives) => RegexOld.oneOfFR(alternatives.map(toKleene))
    case Void(r) => toKleene(r)
  }

  // TODO private?
  import higherkindness.droste.Algebra
  import higherkindness.droste.data.CoattrF
  import higherkindness.droste.data.prelude._
  def ofKleeneAlgebra[A, M](
    matches: (M, A) => Boolean): Algebra[CoattrF[KleeneF, M, ?], RE[A, M, Unit]] = Algebra {
    CoattrF.un(_) match {
      case Left(m) => Elem(m, a => if (matches(m, a)) Some(()) else None)
      case Right(k) =>
        k match {
          case KleeneF.One => Eps
          // TODO greediness
          case KleeneF.Star(r) => r.star(Greediness.Greedy).void
          case KleeneF.Times(l, r) => l *> r
          case KleeneF.Zero => Fail()
          case KleeneF.Plus(l, r) => l | r
        }
    }
  }

  def ofRegex[A: cats.Order](r: Regex[A]): RE[A, regex.Match[A], Unit] =
    higherkindness.droste.scheme.cata(ofKleeneAlgebra[A, regex.Match[A]](_.matches(_))).apply(r)

  // TODO optimize
  // TODO naming/documentation
  // TODO ops class
  def matcher[F[_]:Foldable, In, M, Out](r: RE[In, M, Out]): F[In] => Boolean = {
    val rc = r.void.compile[In]
    fin => rc.parseOnly(fin).isDefined
  }

  // TODO add more stuff to this?
  implicit final class RegexOps[In, M, Out](private val r: RE[In, M, Out]) extends AnyVal {
    def matcher[F[_]:Foldable]: F[In] => Boolean = RE.matcher(r)
  }
}

// TODO remove
//object CodyTesting {
//  import Greediness._
//  import ceedubs.irrec.regex.Match.MatchSet
//
//  type Regex[In, A] = RE[In, Unit, A]
//
//  type RegexM[A] = RE[A, regex.Match[A], Unit]
//
//  def matching[A: cats.Order](m: Match[A]): RegexM[A] =
//    RE.Elem.MatchElem[A](m): RE[A, Match[A], A]
//    //RE.Match(m, a => if (m.matches(a)) Some(()) else None)
//
//  def pred[In](p: In => Boolean): Regex[In, In] = RE.Match((), in => if (p(in)) Some(in) else None)
//
//  val r: Regex[Char, String] = (
//    pred[Char](_.isDigit) | pred(_ === 'a'),
//    pred[Char](_.isUpper).star(NonGreedy),
//    pred[Char](_ == 'A').optional,
//    pred[Char](_.isLower)
//  ).mapN((d, us, uas, l) => d.toString + us.show + uas.show + l.toString)
//
//  val rc: ParseState[Char, String] = RE.compile(r)
//
//  val r2: RE[Char, regex.Match[Char], Unit] =
//    (matching(MatchSet.allow(CharacterClasses.digit)) | matching(Match.lit('a'))) *>
//      matching(MatchSet.allow(CharacterClasses.upperAlpha)).star(Greediness.Greedy) *>
//      matching(Match.lit('A')).optional *>
//      matching(MatchSet.allow(CharacterClasses.lowerAlpha))
//  //).mapN((d, us, uas, l) => d.toString + us.show + uas.show + l.toString)
//  //
//  val r2c: ParseState[Char, Unit] = RE.compile(r2)
//}
