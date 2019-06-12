package ceedubs.irrec
package regex

import cats.{Eval, Foldable, Monad, Monoid}

private[irrec] trait IndexedSeqFoldable[F[x] <: IndexedSeq[x]] extends Foldable[F] {
  override def foldLeft[A, B](fa: F[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

  override def foldRight[A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    Foldable.iterateRight(fa, lb)(f)

  override def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]): B =
    B.combineAll(fa.iterator.map(f))

  override def foldM[G[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G]): G[B] = {
    val length = fa.length

    G.tailRecM((z, 0)) {
      case (b, i) =>
        if (i < length) G.map(f(b, fa(i)))(b => Left((b, i + 1)))
        else G.pure(Right(b))
    }
  }

}
