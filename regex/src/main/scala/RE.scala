package ceedubs.irrec
package regex

import cats.{Alternative, Applicative, Foldable}
import cats.data.{Chain, NonEmptyList, State}
import cats.implicits._

// TODO break things out into other files
// TODO document type parameters (especially M)
// This code was ported (with minor modifications) from https://hackage.haskell.org/package/regex-applicative
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
  // TODO could chnge this to return a natural transformation
  // TODO Stream is deprecated in 2.13, right?
  // TODO use Cont/ContT?
  // TODO return a custom type?
  def compile2[In, M, A, R](
    re: RE[In, (ThreadId, M), A]): Cont[A => Stream[Thread[In, R]]] => Stream[Thread[In, R]] =
    re match {
      case Eps => _.empty(())

      case x @ FMap(r, f) =>
        val rc = compile2[In, M, x.Init, R](r)
        cont => rc(cont.map(_ compose f))

      case Or(alternatives) =>
        val alternativesC = alternatives.map(compile2[In, M, A, R](_)).toList.toStream
        cont => alternativesC.flatMap(_.apply(cont))

      case Match((id, _), p) =>
        cont =>
          // TODO formatting
          Thread
            .Cont[In, R](id, in => p(in).fold(Stream.empty[Thread[In, R]])(cont.nonEmpty(_))) #:: Stream.empty

        // TODO document what's going on here
      case x @ Star(r, g, z, fold) =>
        val rc = compile2[In, M, x.Init, R](r)
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
        val lc = compile2[In, M, x.Init => A, R](l)
        val rc = compile2[In, M, x.Init, R](r)
        _ match {
          case Cont.Single(f) => lc(Cont.Single(lVal => rc(Cont.Single(f compose lVal))))
          case Cont.Choice(whenEmpty, whenNonEmpty) =>
            lc(
              Cont.Choice(
                whenEmpty = lVal =>
                  rc(Cont.Choice(whenEmpty compose lVal, whenNonEmpty compose lVal)),
                // TODO original code uses Choice here, but Single makes sense, right?
                //whenNonEmpty = lVal => rc(Cont.Single(whenNonEmpty compose lVal))
                whenNonEmpty = lVal =>
                  rc(Cont.Choice(whenNonEmpty compose lVal, whenNonEmpty compose lVal))
              ))
        }
    }

  // TODO
  def kompile[In, M, A](r: RE[In, M, A]): ParseState[In, A] = {
    val threads =
      RE.compile2(assignThreadIds(r)).apply(Cont.Single((a: A) => Stream(Thread.Accept[In, A](a))))
    ParseState.fromThreads(threads)
  }
}

// TODO?
final case class ThreadId(asInt: Int) extends AnyVal

sealed abstract class Thread[In, A] extends Product with Serializable {
  def result: Option[A] = this match {
    case Thread.Accept(value) => Some(value)
    case _ => None
  }
}

object Thread {
  final case class Accept[In, A](value: A) extends Thread[In, A]
  final case class Cont[In, A](id: ThreadId, cont: In => Stream[Thread[In, A]])
      extends Thread[In, A]
}

sealed abstract class Cont[+A] extends Product with Serializable {
  import Cont._

  def empty: A = this match {
    case Single(a) => a
    case Choice(whenEmpty, _) => whenEmpty
  }

  def nonEmpty: A = this match {
    case Single(a) => a
    case Choice(_, whenNonEmpty) => whenNonEmpty
  }

  def map[B](f: A => B): Cont[B] = this match {
    case Single(value) => Single(f(value))
    case Choice(whenEmpty, whenNonEmpty) => Choice(f(whenEmpty), f(whenNonEmpty))
  }
}

object Cont {
  final case class Single[+A](value: A) extends Cont[A]
  final case class Choice[+A](whenEmpty: A, whenNonEmpty: A) extends Cont[A]
}

sealed abstract class Greediness extends Product with Serializable

object Greediness {
  case object Greedy extends Greediness
  case object NonGreedy extends Greediness
}

// TODO should this use a stream instead of a List?
final case class StateQueue[A](reversedElements: List[A], ids: Set[Int]) {
  def insertUnique(id: Int, element: A): StateQueue[A] =
    if (ids.contains(id)) this else StateQueue(element :: reversedElements, ids + id)

  def insertWithoutId(element: A): StateQueue[A] = StateQueue(element :: reversedElements, ids)
}

object StateQueue {
  def empty[A]: StateQueue[A] = StateQueue(Nil, Set.empty)
}

// TODO figure out where to put methods/data that are user-facing vs internal
final case class ParseState[In, A](queue: StateQueue[Thread[In, A]]) extends AnyVal {
  def threads: List[Thread[In, A]] = queue.reversedElements.reverse

  def step(x: In): ParseState[In, A] =
    // TODO would it make more sense to use another type of data structure?
    threads.foldLeft(ParseState.empty[In, A]) { (st, thread) =>
      thread match {
        case Thread.Accept(_) => st
        case Thread.Cont(_, cont) =>
          cont(x).foldLeft(st)(_.addThread(_))
      }
    }

  // TODO maybe don't put the methods that you don't expect people to call directly on here?
  // TODO also is this even needed?
  def addThread(t: Thread[In, A]): ParseState[In, A] = t match {
    case Thread.Accept(_) => ParseState(queue.insertWithoutId(t))
    case Thread.Cont(id, _) => ParseState(queue.insertUnique(id.asInt, t))
  }

  def results: List[A] = threads.flatMap(_.result)

  // TODO document
  // TODO use foldLeftM to short-circuit? I don't know if this will work
  def anchoredMatch[F[_]](input: F[In])(implicit foldableF: Foldable[F]): Option[A] =
    input.foldLeft(this)(_.step(_)).results.headOption
}

object ParseState {
  def empty[In, A]: ParseState[In, A] = ParseState(StateQueue.empty)

  def fromThreads[F[_], In, A](threads: F[Thread[In, A]])(
    implicit foldableF: Foldable[F]): ParseState[In, A] =
    threads.foldLeft(empty[In, A])(_.addThread(_))
}

object CodyTesting {
  import Greediness._

  type Regex[In, A] = RE[In, Unit, A]

  def pred[In](p: In => Boolean): Regex[In, In] = RE.Match((), in => if (p(in)) Some(in) else None)

  val r: Regex[Char, String] = (
    pred[Char](_.isDigit) | pred(_ === 'a'),
    pred[Char](_.isUpper).star(NonGreedy),
    pred[Char](_ == 'A').optional,
    pred[Char](_.isLower)
  ).mapN((d, us, uas, l) => d.toString + us.show + uas.show + l.toString)

  val rc: ParseState[Char, String] = RE.kompile(r)
}
