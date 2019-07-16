package ceedubs.irrec
package regex

final case class CapturingKleeneOps[L, A](private val value: CapturingKleene[L, A]) extends AnyVal {
  def |(o: CapturingKleene[L, A]): CapturingKleene[L, A] = CapturingKleene.or(value, o)

  def *(o: CapturingKleene[L, A]): CapturingKleene[L, A] = CapturingKleene.andThen(value, o)
}
