package ceedubs.irrec
package regex

final case class LabeledKleene[L, A](label: Option[L], regex: Kleene[A]) {
  def toCapturingKleene: CapturingKleene[L, A] = CapturingKleene.labeledKleene(this)
}
