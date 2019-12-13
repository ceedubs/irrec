package ceedubs.irrec
package regex

import cats.free.FreeApplicative
import cats.data.Chain

object CapturingKleeneA {
  def lift[M, In](k: Kleene[M]): CapturingKleeneA[M, In, Chain[In]] =
    FreeApplicative.lift(CaptureGroup(k, identity))
}
