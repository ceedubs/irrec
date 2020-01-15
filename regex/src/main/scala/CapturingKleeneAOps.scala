package ceedubs.irrec
package regex

import cats.Foldable

final class CapturingKleeneAOps[M, In, Out](private val ck: CapturingKleeneA[M, In, Out])
    extends AnyVal {
  def matcher[F[_]](doMatch: (M, In) => Boolean)(
    implicit foldableF: Foldable[F]): F[In] => Option[Out] =
    CapturingKleeneA.matcher(ck, doMatch)
}
