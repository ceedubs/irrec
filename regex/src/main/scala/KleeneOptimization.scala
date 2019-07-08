package ceedubs.irrec
package regex

import Regex._
import KleeneF._

import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.{Coattr, CoattrF}, Coattr.Roll
import CoattrF.{Roll => RollF}
import higherkindness.droste.data.prelude._

object KleeneOptimization {
  def partialOptimizeKleene[A]: PartialFunction[CoattrF[KleeneF, A, Kleene[A]], Kleene[A]] = {
    case RollF(Times(Roll(One), x)) => x
    case RollF(Times(x, Roll(One))) => x
    case RollF(Plus(x, Roll(Zero))) => x
    case RollF(Plus(Roll(Zero), x)) => x
    case RollF(Star(Roll(Plus(l, Roll(KleeneF.One))))) =>
      or(star(l), empty)
    case RollF(Star(Roll(Plus(Roll(KleeneF.One), r)))) =>
      or(empty, star(r))
    case RollF(Star(x @ Roll(One))) => x
    case RollF(Star(x @ Roll(Star(_)))) => x
    // TODO should be able to use an `Eq` instance and compare for OR of any 2 things that are equal?
    case RollF(Plus(x @ Roll(One), Roll(One))) => x
  }

  def optimizeKleeneAlgebra[A]: Algebra[CoattrF[KleeneF, A, ?], Kleene[A]] = Algebra {
    val pf = partialOptimizeKleene[A]
    val default: CoattrF[KleeneF, A, Kleene[A]] => Kleene[A] = x => Coattr(CoattrF.un(x))
    k => pf.applyOrElse(k, default)
  }

  def optimizeKleene[A]: Kleene[A] => Kleene[A] = scheme.cata(optimizeKleeneAlgebra[A])
}
