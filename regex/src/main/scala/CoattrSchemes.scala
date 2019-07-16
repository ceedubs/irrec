package ceedubs.irrec
package regex

import cats.Functor
import cats.data.State
import higherkindness.droste.AlgebraM
import higherkindness.droste.data.{Coattr, CoattrF}

object CoattrSchemes {

  def indexLeaves[F[_]: Functor, A]
    : AlgebraM[State[Int, ?], CoattrF[F, A, ?], Coattr[F, (Int, A)]] =
    AlgebraM {
      CoattrF.un(_) match {
        case Left(a) => State((i: Int) => (i + 1, Coattr.pure(i -> a)))
        case Right(z) => State.pure(Coattr.roll(z))
      }
    }
}
