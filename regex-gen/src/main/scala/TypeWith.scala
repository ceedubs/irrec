package ceedubs.irrec
package regex
package gen

/**
 * As presented by Erik Osheim https://www.youtube.com/watch?v=O78hnJuzQwA
 */
sealed abstract class TypeWith[Ev[_]] {
  type T
  def evidence: Ev[T]
}

object TypeWith {
  type Aux[Ev[_], A] = TypeWith[Ev] { type T = A }

  def apply[Ev[_], A](implicit ev: Ev[A]): TypeWith[Ev] = new TypeWith[Ev] {
    type T = A
    def evidence = ev
  }
}
