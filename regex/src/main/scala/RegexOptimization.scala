package ceedubs.irrec
package regex

import Regex._
import Match._
import KleeneF._

import cats.kernel.Order
import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.{Coattr, CoattrF}
import Coattr.{Pure, Roll}
import CoattrF.{Pure => PureF, Roll => RollF}
import higherkindness.droste.data.prelude._
import cats.collections.Discrete

object RegexOptimization {
  def partialOptimizeRegex[A: Discrete: Order]
    : PartialFunction[CoattrF[KleeneF, Match[A], Regex[A]], Regex[A]] = {
    case PureF(MatchSet.Forbid(d)) if d.isEmpty => Regex.wildcard
    case PureF(MatchSet.Allow(d)) if d.isEmpty => Regex.impossible
    case RollF(Plus(Pure(x), Pure(y))) => matching(Match.unionMatches(x, y))
    case RollF(Plus(Pure(x), Roll(Plus(Pure(y), z)))) =>
      matching(Match.unionMatches(x, y)) | z
    case RollF(Plus(Roll(Plus(Pure(x), y)), Pure(z))) =>
      matching(Match.unionMatches(x, z)) | y
    case RollF(Plus(Roll(Plus(Pure(l1), l2)), Roll(Plus(Pure(r1), r2)))) =>
      matching(Match.unionMatches(l1, r1)) | (l2 | r2)
    case RollF(Plus(Roll(Plus(x @ Pure(_), y)), z)) => x | (y | z)
    case RollF(Plus(l @ Roll(_), Roll(Plus(r1 @ Pure(_), r2)))) => r1 | (l | r2)
    case RollF(Plus(l @ Roll(_), r @ Pure(_))) => r | l
  }

  def optimizeRegexAlgebra[A: Discrete: Order]: Algebra[CoattrF[KleeneF, Match[A], ?], Regex[A]] =
    Algebra[CoattrF[KleeneF, Match[A], ?], Regex[A]] {
      val pf = KleeneOptimization.partialOptimizeKleene[Match[A]] orElse partialOptimizeRegex[A]
      val default: CoattrF[KleeneF, Match[A], Regex[A]] => Regex[A] = x => Coattr(CoattrF.un(x))
      k => pf.applyOrElse(k, default)
    }

  def optimizeRegex[A: Discrete: Order]: Regex[A] => Regex[A] = scheme.cata(optimizeRegexAlgebra[A])
}
