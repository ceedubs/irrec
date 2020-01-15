package ceedubs.irrec
package regex

import higherkindness.droste.data.Coattr

// TODO do we really want to support `or` on capture groups?
object CapturingKleene {
  def labeledKleene[L, A](value: LabeledKleene[L, A]): CapturingKleene[L, A] =
    Coattr.pure(value)

  def or[L, A](x: CapturingKleene[L, A], y: CapturingKleene[L, A]): CapturingKleene[L, A] =
    Coattr.roll(SemirngF.Plus(x, y))

  def andThen[L, A](x: CapturingKleene[L, A], y: CapturingKleene[L, A]): CapturingKleene[L, A] =
    Coattr.roll(SemirngF.Times(x, y))
}
