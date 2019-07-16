package ceedubs.irrec
package regex

sealed abstract class SemirngF[+A] extends Product with Serializable

object SemirngF {
  final case class Times[+A](l: A, r: A) extends SemirngF[A]
  final case class Plus[+A](l: A, r: A) extends SemirngF[A]
}
