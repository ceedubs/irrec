package ceedubs.irrec
package regex
package applicative

sealed abstract class Cont[+A] extends Product with Serializable {
  import Cont._

  def empty: A = this match {
    case Single(a) => a
    case Choice(whenEmpty, _) => whenEmpty
  }

  def nonEmpty: A = this match {
    case Single(a) => a
    case Choice(_, whenNonEmpty) => whenNonEmpty
  }

  def map[B](f: A => B): Cont[B] = this match {
    case Single(value) => Single(f(value))
    case Choice(whenEmpty, whenNonEmpty) => Choice(f(whenEmpty), f(whenNonEmpty))
  }
}

object Cont {
  final case class Single[+A](value: A) extends Cont[A]
  final case class Choice[+A](whenEmpty: A, whenNonEmpty: A) extends Cont[A]
}
