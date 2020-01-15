package ceedubs.irrec
package regex

import cats.data.Chain

// TODO create a separate package for capturing
// TODO document
abstract class CaptureGroup[M, In, A] {
  def kleene: Kleene[M]

  def mapCaptured(captured: Chain[In]): A
}

object CaptureGroup {
  def apply[M, In, A](k: Kleene[M], f: Chain[In] => A): CaptureGroup[M, In, A] =
    new CaptureGroup[M, In, A] {
      def kleene: Kleene[M] = k
      def mapCaptured(captured: Chain[In]): A = f(captured)
    }
}
