package ceedubs.irrec
package regex

import cats.implicits._
import cats.{Foldable, Order}
import scala.collection.immutable.{SortedMap, SortedSet}

final case class NFA[A, I](
  initStates: SortedSet[I],
  finalStates: SortedSet[I],
  transitions: SortedMap[I, List[(I, A)]])

object NFA {

  def runNFA[F[_], A, I](nfa: NFA[Match[A], I])(implicit orderingA: Ordering[A], orderingI: Ordering[I], foldableF: Foldable[F]): F[A] => Boolean = {
    implicit val orderA: Order[A] = Order.fromOrdering(orderingA)
    (fa: F[A]) =>
      val finalStates: SortedSet[I] = fa.foldLeft(nfa.initStates)((currentStates, a) =>
        currentStates
        .flatMap(i =>
          nfa.transitions.getOrElse(i, List.empty)
          .collect{ case (i, m) if m.matches(a) => i }
        )
      )
      nfa.finalStates.exists(finalStates.contains(_))
  }
}
