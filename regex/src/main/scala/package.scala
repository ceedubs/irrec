package ceedubs.irrec

import qq.droste.data.{Mu, CoattrF}

package object regex {
  type Free[F[_], A] = Mu[CoattrF[F, A, ?]]
  type Kleene[A] = Free[KleeneF, A]
  type Regex[A] = Kleene[Match[A]]

  implicit def toKleeneOps[A](r: Kleene[A]): KleeneOps[A] = KleeneOps(r)

  implicit def toRegexOps[A](r: Regex[A]): RegexOps[A] = RegexOps(r)

  implicit def toCharRegexOps(r: Regex[Char]): CharRegexOps = CharRegexOps(r)
}
