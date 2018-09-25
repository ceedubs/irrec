package ceedubs.irrec

import qq.droste.data.{Mu, CoattrF}

package object regex {
  type Free[F[_], A] = Mu[CoattrF[F, A, ?]]
  type Regex[A] = Free[KleeneF, Match[A]]

  implicit def toRegexOps[A](r: Regex[A]): RegexOps[A] = RegexOps(r)

  implicit def toCharRegexOps(r: Regex[Char]): CharRegexOps = CharRegexOps(r)
}
