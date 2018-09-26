package ceedubs.irrec
package regex

import cats.implicits._
import cats.Foldable
import scala.collection.immutable.{SortedMap, SortedSet}

final case class NFA[I, A](
  initStates: SortedSet[I],
  finalStates: SortedSet[I],
  transitions: SortedMap[I, List[(I, A)]])

object NFA {

  def runNFA[F[_], I, B, A](nfa: NFA[I, B], matches: (B, A) => Boolean)(implicit orderingI: Ordering[I], foldableF: Foldable[F]): F[A] => Boolean = {
    (fa: F[A]) =>
      val finalStates: SortedSet[I] = fa.foldLeft(nfa.initStates)((currentStates, a) =>
        currentStates
        .flatMap(i =>
          nfa.transitions.getOrElse(i, List.empty)
          .collect{ case (i, b) if matches(b, a) => i }
        )
      )
      nfa.finalStates.exists(finalStates.contains(_))
  }
}
