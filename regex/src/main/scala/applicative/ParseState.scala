package ceedubs.irrec
package regex
package applicative

import cats.Foldable
import cats.evidence.Is
import cats.implicits._

// TODO figure out where to put methods/data that are user-facing vs internal
// TODO consider using something like Stream in some places to get lazy behavior
final case class ParseState[In, A](queue: StateQueue[Thread[In, A]]) extends AnyVal {
  import ParseState._

  def threads: List[Thread[In, A]] = queue.reversedElements.reverse

  def step(x: In): ParseState[In, A] =
    // TODO would it make more sense to use another type of data structure?
    threads.foldLeft(ParseState.empty[In, A]) { (st, thread) =>
      thread match {
        case Thread.Accept(_) => st
        case Thread.Cont(_, cont) =>
          cont(x).foldLeft(st)(_.addThread(_))
      }
    }

  // TODO maybe don't put the methods that you don't expect people to call directly on here?
  // TODO also is this even needed?
  def addThread(t: Thread[In, A]): ParseState[In, A] = t match {
    case Thread.Accept(_) => ParseState(queue.insertWithoutId(t))
    case Thread.Cont(id, _) => ParseState(queue.insertUnique(id.asInt, t))
  }

  def results: List[A] = threads.flatMap(_.result)

  // TODO document
  // TODO use foldLeftM to short-circuit? I don't know if this will work
  def parseOnly[F[_]](input: F[In])(implicit foldableF: Foldable[F]): Option[A] =
    input.foldLeft(this)(_.step(_)).results.headOption

  // TODO could add with ops class instead?
  def parseOnlyS(input: String)(implicit ev: Char Is In): Option[A] =
    parseOnly(ev.substitute(input: IndexedSeq[Char]))
}

object ParseState {
  def empty[In, A]: ParseState[In, A] = ParseState(StateQueue.empty)

  def fromThreads[F[_], In, A](threads: F[Thread[In, A]])(
    implicit foldableF: Foldable[F]): ParseState[In, A] =
    threads.foldLeft(empty[In, A])(_.addThread(_))

  implicit private val indexedSeqFoldable: Foldable[IndexedSeq] =
    new IndexedSeqFoldable[IndexedSeq] {}
}
