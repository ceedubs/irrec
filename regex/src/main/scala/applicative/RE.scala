package ceedubs.irrec
package regex
package applicative

// TODO
import ceedubs.irrec.regex.{Regex => RegexOld}

import cats.{Alternative, Applicative}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, State}
import cats.implicits._
import higherkindness.droste.data.Coattr

// This code was ported (with minor modifications) from https://hackage.haskell.org/package/regex-applicative
// TODO document type parameters (especially M)
sealed abstract class RE[-In, +M, A] extends Product with Serializable {
  import RE._

  // TODO add ops on to avoid variance shenanigans?
  // TODO add in optimizations during constructions like this or have a separate method to optimize?
  def |[In2 <: In, M2 >: M](o: RE[In2, M2, A]): RE[In2, M2, A] = (this, o) match {
    case (Or(xs), Or(ys)) => Or(xs ::: ys)
    case (_, Fail()) => this
    case (Fail(), _) => o
    case (Or(xs), _) => Or(o :: xs)
    case (_, Or(ys)) => Or(this :: ys)
    case _ => Or(NonEmptyList(this, o :: Nil))
  }

  def star(greediness: Greediness): RE[In, M, Chain[A]] =
    RE.Star(this, greediness, Chain.empty[A], (as: Chain[A], a: A) => as.append(a))

  def many: RE[In, M, Chain[A]] = star(Greediness.Greedy)

  def few: RE[In, M, Chain[A]] = star(Greediness.NonGreedy)

  // TODO document
  def oneOrMore(greediness: Greediness): RE[In, M, NonEmptyChain[A]] =
    this.map2(this.star(greediness))(NonEmptyChain.fromChainPrepend(_, _))

  def count(n: Int): RE[In, M, Chain[A]] =
    Chain.fromSeq(1 to n).traverse(_ => this)

  // TODO is this handling greediness right? Test this.
  def repeat(minInclusive: Int, maxInclusive: Option[Int], greediness: Greediness): RE[In, M, Chain[A]] = {
    val tail = maxInclusive.fold(star(greediness).some){ max =>
      (1 to (max - minInclusive)).toList.toNel.map { counts =>
        val orderedCounts = greediness match {
          case Greediness.Greedy => counts.reverse
          case Greediness.NonGreedy => counts
        }
        // TODO this is a mess
        Or(orderedCounts.map(i => count(i)))
      }
    }
    val head = count(minInclusive)
    tail.fold(head)(tail => head.map2(tail)(_ concat _))
  }

  def optional[In2 <: In, M2 >: M]: RE[In2, M2, Option[A]] =
    this.map[Option[A]](Some(_)) | none[A].pure[RE[In2, M2, ?]]

  def map[B](f: A => B): RE[In, M, B] = FMap(this, f)
}

object RE {
  case object Eps extends RE[Any, Nothing, Unit]
  final case class Fail[A]() extends RE[Any, Nothing, A]
  // TODO should this actually have both? Instead could just rely on doing a `map` and changing `M`.
  // But that assumes that the conversion will be the same for all M
  // Also that's probably going to mess with type inference
  final case class Match[-In, +M, A](m: M, f: In => Option[A]) extends RE[In, M, A]
  final case class AndThen[-In, +M, I, A](l: RE[In, M, I => A], r: RE[In, M, I])
      extends RE[In, M, A] {
    type Init = I
  }
  // TODO use a lazy structure like NonEmptyStream?
  final case class Or[-In, +M, A](alternatives: NonEmptyList[RE[In, M, A]]) extends RE[In, M, A]
  final case class FMap[-In, +M, I, A](r: RE[In, M, I], f: I => A) extends RE[In, M, A] {
    type Init = I
  }
  final case class Star[-In, +M, I, A](
    r: RE[In, M, I],
    greediness: Greediness,
    z: A,
    fold: (A, I) => A)
      extends RE[In, M, A] {
    type Init = I
  }
  // TODO efficiently handle with NFA
  //final case class Void[-In, +M, I](r: RE[In, M, I]) extends RE[In, M, Unit] {
  //  type Init = I
  //}

  // TODO document
  def traverseM[F[_], In, M, M2, A](re: RE[In, M, A])(f: M => F[M2])(
    implicit F: Applicative[F]): F[RE[In, M2, A]] = re match {
    case Match(m, p) => f(m).map(Match(_, p))
    case Eps => F.pure(Eps)
    case x @ Fail() => F.pure(x)
    case Star(r, g, z, fold) => traverseM(r)(f).map(Star(_, g, z, fold))
    case FMap(r, g) => traverseM(r)(f).map(FMap(_, g))
    case Or(alternatives) => alternatives.traverse(traverseM(_)(f)).map(Or(_))
    case AndThen(l, r) => traverseM(l)(f).map2(traverseM(r)(f))(AndThen(_, _))
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
    re: RE[In, (ThreadId, M), A]): Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]] =
    re match {
      case Eps => _.empty(())

      case x @ FMap(r, f) =>
        val rc = compileCont[In, M, x.Init, R](r)
        cont => rc(cont.map(_ compose f))

      case Or(alternatives) =>
        val alternativesC = alternatives.map(compileCont[In, M, A, R](_)).toList.toStream
        cont => alternativesC.flatMap(_.apply(cont))

      case Match((id, _), p) =>
        cont =>
          // TODO formatting
          Thread
            .Cont[In, R](id, in => p(in).fold(Stream.empty[Thread[In, R]])(cont.nonEmpty(_))) #:: Stream.empty

        // TODO document what's going on here
      case x @ Star(r, g, z, fold) =>
        val rc = compileCont[In, M, x.Init, R](r)
        def threads(z: A, cont: Cont[A => Stream[Thread[In, R]]]): Stream[Thread[In, R]] = {
          def stop = cont.empty(z)
          // TODO think more about laziness
          def go =
            rc(Cont.Choice(whenEmpty = _ => Stream.empty, whenNonEmpty = { v =>
              threads(fold(z, v), Cont.Single(cont.nonEmpty))
            }))
          g match {
            case Greediness.Greedy => go #::: stop
            case Greediness.NonGreedy => stop #::: go
          }
        }
        threads(z, _)
      case Fail() => _ => Stream.empty

      case x @ AndThen(l, r) =>
        val lc = compileCont[In, M, x.Init => A, R](l)
        val rc = compileCont[In, M, x.Init, R](r)
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

  // TODO
  def compile[In, M, A](r: RE[In, M, A]): ParseState[In, A] = {
    val threads =
      RE.compileCont(assignThreadIds(r))
        .apply(Cont.Single((a: A) => Stream(Thread.Accept[In, A](a))))
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
    case RE.Match(m, _) => Coattr.pure(m)
    case Or(alternatives) => RegexOld.oneOfFR(alternatives.map(toKleene))
  }

  // TODO private?
  import higherkindness.droste.Algebra
  import higherkindness.droste.data.CoattrF
  import higherkindness.droste.data.prelude._
  def ofKleeneAlgebra[A, M](
    matches: (M, A) => Boolean): Algebra[CoattrF[KleeneF, M, ?], RE[A, M, Unit]] = Algebra {
    CoattrF.un(_) match {
      case Left(m) => RE.Match(m, a => if (matches(m, a)) Some(()) else None)
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
}

object CodyTesting {
  import Greediness._
  import ceedubs.irrec.regex.Match.MatchSet

  type Regex[In, A] = RE[In, Unit, A]

  type RegexM[A] = RE[A, regex.Match[A], Unit]

  def matching[A: cats.Order](m: Match[A]): RegexM[A] =
    RE.Match(m, a => if (m.matches(a)) Some(()) else None)

  def pred[In](p: In => Boolean): Regex[In, In] = RE.Match((), in => if (p(in)) Some(in) else None)

  val r: Regex[Char, String] = (
    pred[Char](_.isDigit) | pred(_ === 'a'),
    pred[Char](_.isUpper).star(NonGreedy),
    pred[Char](_ == 'A').optional,
    pred[Char](_.isLower)
  ).mapN((d, us, uas, l) => d.toString + us.show + uas.show + l.toString)

  val rc: ParseState[Char, String] = RE.compile(r)

  val r2: RE[Char, regex.Match[Char], Unit] =
    (matching(MatchSet.allow(CharacterClasses.digit)) | matching(Match.lit('a'))) *>
      matching(MatchSet.allow(CharacterClasses.upperAlpha)).star(Greediness.Greedy) *>
      matching(Match.lit('A')).optional *>
      matching(MatchSet.allow(CharacterClasses.lowerAlpha))
  //).mapN((d, us, uas, l) => d.toString + us.show + uas.show + l.toString)
  //
  val r2c: ParseState[Char, Unit] = RE.compile(r2)
}
