package ceedubs.irrec
package regex

final case class LabeledKleene[L, A](label: L, value: Kleene[A]) {
  def toCapturingKleene: CapturingKleene[L, A] = CapturingKleene.labeledKleene(this)
}
