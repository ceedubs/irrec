package ceedubs.irrec

import cats._
import cats.implicits._

sealed abstract class KleeneF[+A] extends Product with Serializable

object KleeneF {
  final case class Plus[+A](l: A, r: A) extends KleeneF[A]
  final case class Times[+A](l: A, r: A) extends KleeneF[A]
  final case class Star[+A](value: A) extends KleeneF[A]
  case object Zero extends KleeneF[Nothing]
  case object One extends KleeneF[Nothing]

  implicit val traverseKleeneF: Traverse[KleeneF] = new Traverse[KleeneF] {
    def traverse[G[_], A, B](fa: KleeneF[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[KleeneF[B]] =
      fa match {
        case Plus(l, r) => G.map2(f(l), f(r))((lb, rb) => Plus(lb, rb))
        case Times(l, r) => G.map2(f(l), f(r))((lb, rb) => Times(lb, rb))
        case Star(x) => G.map(f(x))(Star(_))
        case Zero => G.pure(Zero)
        case One => G.pure(One)
      }

    def foldLeft[A, B](fa: KleeneF[A], b: B)(f: (B, A) => B): B = fa match {
      case Plus(l, r) => f(f(b, l), r)
      case Times(l, r) => f(f(b, l), r)
      case Star(x) => f(b, x)
      case Zero => b
      case One => b
    }

    def foldRight[A, B](fa: KleeneF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Plus(l, r) => f(l, Eval.defer(f(r, lb)))
        case Times(l, r) => f(l, Eval.defer(f(r, lb)))
        case Star(x) => f(x, lb)
        case Zero => lb
        case One => lb
      }
  }

  implicit def eqKleeneF[A](implicit eqA: Eq[A]): Eq[KleeneF[A]] = Eq.instance {
    case (KleeneF.Plus(l1, r1), KleeneF.Plus(l2, r2)) => l1 === l2 && r1 === r2
    case (KleeneF.Times(l1, r1), KleeneF.Times(l2, r2)) => l1 === l2 && r1 === r2
    case (KleeneF.Star(l), KleeneF.Star(r)) => l === r
    case (KleeneF.One, KleeneF.One) => true
    case (KleeneF.Zero, KleeneF.Zero) => true
    case (_, _) => false
  }
}
