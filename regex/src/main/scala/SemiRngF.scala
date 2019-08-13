package ceedubs.irrec
package regex

import cats.{~>, Applicative, Eval, Traverse}

sealed abstract class SemirngF[+A] extends Product with Serializable {
  import ceedubs.irrec.regex.SemirngF._

  def toKleeneF: KleeneF[A] = this match {
    case Times(l, r) => KleeneF.Times(l, r)
    case Plus(l, r) => KleeneF.Plus(l, r)
  }
}

object SemirngF {
  final case class Times[+A](l: A, r: A) extends SemirngF[A]
  final case class Plus[+A](l: A, r: A) extends SemirngF[A]

  val semirngFToKleeneF: SemirngF ~> KleeneF = λ[SemirngF ~> KleeneF](_.toKleeneF)

  implicit val traverseSemirngF: Traverse[SemirngF] = new Traverse[SemirngF] {
    def traverse[G[_], A, B](fa: SemirngF[A])(f: A => G[B])(
      implicit G: Applicative[G]): G[SemirngF[B]] =
      fa match {
        case Plus(l, r) => G.map2(f(l), f(r))((lb, rb) => Plus(lb, rb))
        case Times(l, r) => G.map2(f(l), f(r))((lb, rb) => Times(lb, rb))
      }

    def foldLeft[A, B](fa: SemirngF[A], b: B)(f: (B, A) => B): B = fa match {
      case Plus(l, r) => f(f(b, l), r)
      case Times(l, r) => f(f(b, l), r)
    }

    def foldRight[A, B](fa: SemirngF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Plus(l, r) => f(l, Eval.defer(f(r, lb)))
        case Times(l, r) => f(l, Eval.defer(f(r, lb)))
      }
  }
}
