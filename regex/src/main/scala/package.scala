package ceedubs.irrec

import higherkindness.droste.data.Coattr

package object regex {
  type Kleene[A] = Coattr[KleeneF, A]
  type Regex[A] = Kleene[Match[A]]
  type CapturingKleene[L, A] = Coattr[SemirngF, LabeledKleene[L, A]]

  implicit def toKleeneOps[A](r: Kleene[A]): KleeneOps[A] = new KleeneOps(r)

  implicit def toRegexOps[A](r: Regex[A]): RegexOps[A] = new RegexOps(r)

  implicit def toCharRegexOps(r: Regex[Char]): CharRegexOps = new CharRegexOps(r)

  implicit def toCapturingKleeneOps[L, A](k: CapturingKleene[L, A]): CapturingKleeneOps[L, A] = new CapturingKleeneOps(k)
}
