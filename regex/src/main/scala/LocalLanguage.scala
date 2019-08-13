package ceedubs.irrec
package regex

import scala.collection.immutable.{SortedMap, SortedSet}
import cats.implicits._

/**
 * @tparam I the type of the index of nodes
 * @tparam A the type of the matches
 */
final case class LocalLanguage[I, A](
  isEmpty: Boolean,
  leading: List[(I, A)],
  trailing: List[(I, A)],
  transitions: SortedMap[I, List[(I, A)]]) {

  // TODO ceedubs test functor laws
  // TODO ceedubs is mapValues bad here?
  def map[B](f: A => B): LocalLanguage[I, B] = this.copy(
    leading = leading.map{ case (i, a) => (i, f(a)) },
    trailing = trailing.map{ case (i, a) => (i, f(a)) },
    transitions = transitions.mapValues(_.map{ case (i, a) => (i, f(a)) }))
}

object LocalLanguage {
  def leaf[I, A](index: I, a: A)(implicit orderingI: Ordering[I]): LocalLanguage[I, A] = {
    val singletonList = List((index, a))
    LocalLanguage(
      isEmpty = false,
      leading = singletonList,
      trailing = singletonList,
      transitions = SortedMap.empty)
  }

  def intLocalLanguageToNFA[A](ll: LocalLanguage[Int, A]): NFA[Int, A] =
    NFA(
      initStates = SortedSet(0),
      finalStates = (if (ll.isEmpty) SortedSet(0) else SortedSet.empty[Int]) |+| ll.trailing
        .map(_._1)
        .to[SortedSet],
      transitions = ll.leading.foldMap { l =>
        SortedMap((0, List(l)))
      } |+| ll.transitions
    )
}
