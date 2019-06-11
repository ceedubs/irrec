package ceedubs.irrec.regex

import cats.Order
import cats.data.NonEmptyList
import cats.implicits._
import ceedubs.irrec.regex.Match.Negated.NegatedRange
import ceedubs.irrec.regex.Match.Negated.NegatedLiteral

sealed abstract class Match[+A] extends Product with Serializable {
  import Match._

  def matches[AA >: A](a: AA)(implicit orderA: Order[AA]): Boolean = this match {
    case Literal(expected) => a === expected
    case Range(l, r) => a >= l && a <= r
    case NoneOf(l) => l.forall(n => !n.toMatch.matches(a))
    case Wildcard => true
  }
}

object Match {
  final case class Literal[+A](value: A) extends Match[A]

  /** An inclusive range */
  final case class Range[+A](lower: A, upper: A) extends Match[A]

  case object Wildcard extends Match[Nothing]

  final case class NoneOf[+A](values: NonEmptyList[Negated[A]]) extends Match[A]

  sealed abstract class Negated[+A] extends Product with Serializable {
    def toMatch: Match[A] = this match {
      case NegatedRange(range) => range
      case NegatedLiteral(lit) => lit
    }
  }

  object Negated {
    final case class NegatedRange[+A](range: Range[A]) extends Negated[A]
    final case class NegatedLiteral[+A](lit: Literal[A]) extends Negated[A]
  }

  def lit[A](a: A): Match[A] = Literal(a)

  def range[A](lower: A, upper: A): Match[A] = Range(lower, upper)

  def wildcard[A]: Match[A] = Wildcard
}
