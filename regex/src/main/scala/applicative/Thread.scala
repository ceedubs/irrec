package ceedubs.irrec
package regex
package applicative

// TODO?
final case class ThreadId(asInt: Int) extends AnyVal

sealed abstract class Thread[In, A] extends Product with Serializable {
  def result: Option[A] = this match {
    case Thread.Accept(value) => Some(value)
    case _ => None
  }
}

object Thread {
  final case class Accept[In, A](value: A) extends Thread[In, A]
  final case class Cont[In, A](id: ThreadId, cont: In => Stream[Thread[In, A]])
      extends Thread[In, A]
}
