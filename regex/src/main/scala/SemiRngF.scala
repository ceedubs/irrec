package ceedubs.irrec
package regex

import cats.~>

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
}
