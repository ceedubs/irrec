package ceedubs.irrec

import cats.free.FreeApplicative
import higherkindness.droste.data.Coattr

package object regex {
  type Kleene[A] = Coattr[KleeneF, A]
  type Regex[A] = Kleene[Match[A]]
  type CapturingKleene[L, A] = Coattr[SemirngF, LabeledKleene[L, A]]
  type CapturingKleeneA[M, In, Out] = FreeApplicative[CaptureGroup[M, In, ?], Out]
  type CapturingRegex[L, A] = CapturingKleene[L, Match[A]]

  implicit def toKleeneOps[A](r: Kleene[A]): KleeneOps[A] = new KleeneOps(r)

  implicit def toCapturingKleeneAOps[M, In, Out](
    r: CapturingKleeneA[M, In, Out]): CapturingKleeneAOps[M, In, Out] = new CapturingKleeneAOps(r)

  implicit def toRegexOps[A](r: Regex[A]): RegexOps[A] = new RegexOps(r)

  implicit def toCharRegexOps(r: Regex[Char]): CharRegexOps = new CharRegexOps(r)

  implicit def toCapturingKleeneOps[L, A](k: CapturingKleene[L, A]): CapturingKleeneOps[L, A] =
    new CapturingKleeneOps(k)
}
