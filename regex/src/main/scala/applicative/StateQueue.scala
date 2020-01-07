package ceedubs.irrec
package regex
package applicative

// TODO should this use a stream instead of a List?
final case class StateQueue[A](reversedElements: List[A], ids: Set[Int]) {
  def insertUnique(id: Int, element: A): StateQueue[A] =
    if (ids.contains(id)) this else StateQueue(element :: reversedElements, ids + id)

  def insertWithoutId(element: A): StateQueue[A] = StateQueue(element :: reversedElements, ids)
}

object StateQueue {
  def empty[A]: StateQueue[A] = StateQueue(Nil, Set.empty)
}
