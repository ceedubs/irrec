package ceedubs.irrec
package regex

import Regex._
import qq.droste.{Algebra, scheme}
import qq.droste.data.{Coattr, CoattrF}
import qq.droste.data.prelude._

object KleeneOptimization {
  // TODO this can be removed if https://github.com/andyscott/droste/pull/79 is merged
  private object Roll {
    def unapply[F[_], A](f: Coattr[F, A]): Option[F[Coattr[F, A]]] = Coattr.un(f) match {
      case Right(fa) => Some(fa)
      case _ => None
    }
  }

  def optimizeKleeneAlgebra[A]: Algebra[CoattrF[KleeneF, A, ?], Kleene[A]] = Algebra{
    CoattrF.un(_) match {
      case l @ Left(_) => Coattr(l)
      case r @ Right(k) => k match {
        case KleeneF.Times(Roll(KleeneF.One), x) => x
        case KleeneF.Times(x, Roll(KleeneF.One)) => x
        case KleeneF.Plus(x, Roll(KleeneF.Zero)) => x
        case KleeneF.Plus(Roll(KleeneF.Zero), x) => x
        case KleeneF.Star(Roll(KleeneF.Plus(l, Roll(KleeneF.One)))) =>
          or(star(l), empty)
        case KleeneF.Star(Roll(KleeneF.Plus(Roll(KleeneF.One), r))) =>
          or(empty, star(r))
        case KleeneF.Star(x @ Roll(KleeneF.One)) => x
        case KleeneF.Star(x @ Roll(KleeneF.Star(_))) => x
        // TODO should be able to use an `Eq` instance and compare for OR of any 2 things that are equal?
        case KleeneF.Plus(x @ Roll(KleeneF.One), Roll(KleeneF.One)) => x
        case _ => Coattr(r)
      }
    }
  }

  def optimizeKleene[A]: Kleene[A] => Kleene[A] = scheme.cata(optimizeKleeneAlgebra[A])
}
