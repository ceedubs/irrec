package ceedubs.irrec.regex

import cats.Order
import cats.implicits._

sealed abstract class Match[+A] extends Product with Serializable {
  import Match._

  def matches[AA >: A](a: AA)(implicit orderA: Order[AA]): Boolean = this match {
    case Literal(expected) => a === expected
    case Range(l, r) => a >= l && a <= r
    case Wildcard => true
  }
}

object Match {
  final case class Literal[+A](value: A) extends Match[A]

  /** An inclusive range */
  final case class Range[+A](lower: A, upper: A) extends Match[A]

  case object Wildcard extends Match[Nothing]

  def lit[A](a: A): Match[A] = Literal(a)

  def range[A](lower: A, upper: A): Match[A] = Range(lower, upper)

  def wildcard[A]: Match[A] = Wildcard
}
