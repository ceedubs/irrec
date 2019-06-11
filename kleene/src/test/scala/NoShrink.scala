package ceedubs.irrec
import org.scalacheck.Gen

/**
 * See [[https://github.com/scalatest/scalatest/issues/584]] for the inspiration for this
 * implementation.
 */
final case class NoShrink[A](value: A) extends AnyVal

object NoShrink {
  def noShrink[A](genA: Gen[A]): Gen[NoShrink[A]] = genA.map(a => NoShrink(a))
}
