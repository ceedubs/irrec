package ceedubs.irrec
package regex

import cats.{~>, Alternative, Applicative, Id}
import cats.data.State
import cats.implicits._

// TODO think about variance
sealed abstract class Regex2[F[_], -In, Out] extends Serializable {
  //def fold[In2 <: In, R](
  //  eps: () => R,
  //  fail: () => R,
  //  elem: F[In2 => Option[Out]] => R,
  //  andThen: λ[i => (Regex2[F, In2, i => Out], Regex2[F, In2, i])] ~> λ[a => R]): R = ???
}

object Regex2 {

  final case class Elem[F[_], In, Out](apply: F[In => Option[Out]]) extends Regex2[F, In, Out]

  final case class Eps[F[_]]() extends Regex2[F, Any, Unit]

  final case class Fail[F[_], A]() extends Regex2[F, Any, A]

  final case class AndThen[F[_], -In, I, Out](l: Regex2[F, In, I => Out], r: Regex2[F, In, I])
      extends Regex2[F, In, Out] {
    type Init = I
  }

  final case class FMap[F[_], -In, I, Out](r: Regex2[F, In, I], f: I => Out)
      extends Regex2[F, In, Out] {
    type Init = I
  }

  def eps[F[_], In]: Regex2[F, In, Unit] = Eps()

  implicit def regex2Alternative[F[_], In]: Alternative[Regex2[F, In, ?]] =
    new Alternative[Regex2[F, In, ?]] {
      def ap[A, B](ff: Regex2[F, In, A => B])(fa: Regex2[F, In, A]): Regex2[F, In, B] =
        AndThen(ff, fa)
      def pure[A](x: A): Regex2[F, In, A] = FMap(eps, (_: Unit) => x)
      def combineK[A](x: Regex2[F, In, A], y: Regex2[F, In, A]): Regex2[F, In, A] = ???
      def empty[A]: Regex2[F, In, A] = Fail()
    }

  def transformM[F[_], G[_], M[_], In](f: F ~> λ[a => M[G[a]]])(
    implicit M: Applicative[M]): Regex2[F, In, ?] ~> λ[a => M[Regex2[G, In, a]]] =
    new (Regex2[F, In, ?] ~> λ[a => M[Regex2[G, In, a]]]) { outer =>
      def apply[A](fa: Regex2[F, In, A]): M[Regex2[G, In, A]] = fa match {
        case Elem(x) => f(x).map(Elem(_))
        case _: Eps[_] => M.pure(Eps())
        case Fail() => M.pure(Fail())
        case x @ AndThen(l, r) => outer(l).map2(outer(r))(AndThen[G, In, x.Init, A](_, _))
        case x @ FMap(r, f) => outer(r).map(FMap[G, In, x.Init, A](_, f))
      }
    }

  // TODO use Eval for trampolining?
  def transform[F[_], G[_], In](f: F ~> G): Regex2[F, In, ?] ~> Regex2[G, In, ?] =
    transformM[F, G, Id, In](f)

  def assignThreadIds[F[_], In, A](
    re: Regex2[F, In, A]): Regex2[λ[a => (ThreadId, F[a])], In, A] = {
    val freshId: State[ThreadId, ThreadId] = State(id => (ThreadId(id.asInt + 1), id))
    val withIdS: F ~> λ[a => State[ThreadId, (ThreadId, F[a])]] =
      new (F ~> λ[a => State[ThreadId, (ThreadId, F[a])]]) {
        def apply[B](fb: F[B]): State[ThreadId, (ThreadId, F[B])] = freshId.map(id => (id, fb))
      }
    transformM[F, λ[a => (ThreadId, F[a])], State[ThreadId, ?], In](withIdS)
      .apply(re)
      .runA(ThreadId(0))
      .value
  }
}
