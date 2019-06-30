package ceedubs.irrec.regex

import cats.Order
import cats.collections.{Diet, Discrete, Range}
import cats.implicits._

sealed abstract class Match[A] extends Product with Serializable {
  import Match._

  def matches(a: A)(implicit orderA: Order[A]): Boolean = this match {
    case Literal(expected) => a === expected
    case MatchSet(t) => t.contains(a)
    case NegatedMatchSet(t) => !t.contains(a)
    case Wildcard() => true
  }
}

object Match {

  final case class Literal[A](value: A) extends Match[A]

  final case class MatchSet[A](diet: Diet[A]) extends Match[A] {
    def negate: NegatedMatchSet[A] = NegatedMatchSet(diet)

    def union(other: MatchSet[A])(implicit discreteA: Discrete[A], orderA: Order[A]): MatchSet[A] =
      MatchSet(diet | other.diet)
  }

  // TODO ceedubs reconsider whether these methods should live here. Maybe just add more helpers for Diet.
  object MatchSet {
    def range[A](lower: A, upper: A): MatchSet[A] =
      MatchSet(Diet.fromRange(Range(lower, upper)))

    def one[A](a: A): MatchSet[A] =
      MatchSet(Diet.one(a))
  }

  final case class NegatedMatchSet[A](diet: Diet[A]) extends Match[A]

  sealed case class Wildcard[A]() extends Match[A]

  def lit[A](a: A): Match[A] = Literal(a)

  def range[A](lower: A, upper: A): Match[A] =
    MatchSet.range(lower, upper)

  def wildcard[A]: Match[A] = Wildcard()
}
