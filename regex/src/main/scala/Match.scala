package ceedubs.irrec.regex

import cats.Order
import cats.collections.{Diet, Discrete}
import cats.implicits._

sealed abstract class Match[A] extends Product with Serializable {
  import Match._

  def matches(a: A)(implicit orderA: Order[A]): Boolean = this match {
    case Literal(expected) => a === expected
    case MatchSet.Allow(allowed) => allowed.contains(a)
    case MatchSet.Forbid(forbidden) => !forbidden.contains(a)
    case Wildcard() => true
  }
}

object Match {

  final case class Literal[A](value: A) extends Match[A]

  sealed abstract class MatchSet[A] extends Match[A] {
    import MatchSet._

    def union(other: MatchSet[A])(implicit discreteA: Discrete[A], orderA: Order[A]): MatchSet[A] =
      (this, other) match {
        case (Allow(x), Allow(y)) => Allow(x | y)
        case (Allow(x), Forbid(y)) => Forbid(y -- x)
        case (Forbid(x), Allow(y)) => Forbid(x -- y)
        case (Forbid(x), Forbid(y)) => Forbid(x & y)
      }

    def intersect(
      other: MatchSet[A])(implicit discreteA: Discrete[A], orderA: Order[A]): MatchSet[A] =
      (this, other) match {
        case (Allow(x), Allow(y)) => Allow(x & y)
        case (Allow(x), Forbid(y)) => Allow(x -- y)
        case (Forbid(x), Allow(y)) => Allow(y -- x)
        case (Forbid(x), Forbid(y)) => Forbid(x | y)
      }

    def negate: MatchSet[A] = this match {
      case Allow(x) => Forbid(x)
      case Forbid(x) => Allow(x)
    }
  }

  object MatchSet {
    final case class Allow[A](allowed: Diet[A]) extends MatchSet[A]
    final case class Forbid[A](forbidden: Diet[A]) extends MatchSet[A]

    def allow[A](allowed: Diet[A]): MatchSet[A] = Allow(allowed)

    def forbid[A](forbidden: Diet[A]): MatchSet[A] = Forbid(forbidden)
  }

  sealed case class Wildcard[A]() extends Match[A]

  def lit[A](a: A): Match[A] = Literal(a)

  def wildcard[A]: Match[A] = Wildcard()
}
