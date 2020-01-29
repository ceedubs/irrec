package ceedubs.irrec
package regex
package gen

import cats.{Monad, StackSafeMonad}
import org.scalacheck.Gen

private[irrec] object ScalacheckSupport {

  /**
   * CAUTION: this isn't actually stack-safe. But we are working with a version of Scalacheck that
   * doesn't expose the necessary pieces to have a properly stack-safe monad.
   */
  implicit val monadGen: Monad[Gen] = new StackSafeMonad[Gen] {
    def pure[A](a: A): Gen[A] = Gen.const(a)

    def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }
}
