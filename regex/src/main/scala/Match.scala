package ceedubs.irrec.regex

import cats.Order
import cats.collections.{Diet, Discrete}
import cats.implicits._

sealed abstract class Match[A] extends Product with Serializable {
  import Match._

  def matches(a: A)(implicit orderA: Order[A]): Boolean = this match {
    case Literal(expected) => a === expected
    case MatchSet(pos, neg) => pos.forall(_.contains(a)) && !neg.contains(a)
    case Wildcard() => true
  }
}

object Match {

  final case class Literal[A](value: A) extends Match[A]

  final case class MatchSet[A] private(positive: Option[Diet[A]], negative: Diet[A]) extends Match[A] {

    def union(other: MatchSet[A])(implicit discreteA: Discrete[A], orderA: Order[A]): MatchSet[A] = {
      // TODO ceedubs is this right? Document.
      val newPositive = (this.positive, other.positive) match {
        case (None, _) => None
        case (_, None) => None
        case (Some(l), Some(r)) => Some(l | r)
      }
      val negativeIntersection = this.negative & other.negative
      MatchSet(
      positive = newPositive,
      negative = newPositive.fold(negativeIntersection)(negativeIntersection -- _))
    }

    def intersect(other: MatchSet[A])(implicit discreteA: Discrete[A], orderA: Order[A]): MatchSet[A] = {
      MatchSet(
      // TODO ceedubs do I need to subtract newNegative?
      positive = (this.positive, other.positive) match {
        case (None, r) => r
        case (l, None) => l
        case (Some(l), Some(r)) => Some(l & r)
      },
      negative = this.negative | other.negative)
    }

    // TODO ceedubs remove?
    // TODO ceedubs is this right?
    // I think that this works on "primitive" match sets (one side is empty), but I don't know about complex ones
    def negate: MatchSet[A] = MatchSet[A](
      positive = if (negative.isEmpty) None else Some(negative),
      negative = positive.getOrElse(Diet.empty))
  }

  // TODO ceedubs more helpful constructor methods
  object MatchSet {
    // overriding so it is private
    private def apply[A](positive: Option[Diet[A]], negative: Diet[A]): MatchSet[A] = new MatchSet(positive, negative)

    def allow[A](allowed: Diet[A]): MatchSet[A] = MatchSet(Some(allowed), Diet.empty)

    def forbid[A](forbidden: Diet[A]): MatchSet[A] = MatchSet(None, forbidden)
  }

  sealed case class Wildcard[A]() extends Match[A]

  def lit[A](a: A): Match[A] = Literal(a)

  def wildcard[A]: Match[A] = Wildcard()
}
