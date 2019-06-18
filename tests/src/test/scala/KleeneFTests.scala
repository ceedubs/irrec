package ceedubs.irrec

import cats.Traverse
import cats.laws.discipline.{SerializableTests, TraverseTests}
import org.scalacheck.{Arbitrary, Gen}

class KleeneFTests extends IrrecSuite {
  import KleeneFTests._

  checkAll("KleeneF", TraverseTests[KleeneF].traverse[Int, Int, Int, Set[Int], Option, Option])
  checkAll("Traverse[KleeneF]", SerializableTests.serializable(Traverse[KleeneF]))
}

object KleeneFTests {
  def genKleeneF[A](genA: Gen[A], includeZero: Boolean): Gen[KleeneF[A]] =
    Gen.frequency(
      2 -> genA.flatMap(a1 => genA.map(a2 => KleeneF.Times(a1, a2))),
      2 -> genA.flatMap(a1 => genA.map(a2 => KleeneF.Plus(a1, a2))),
      1 -> genA.map(KleeneF.Star(_)),
      1 -> Gen.const(KleeneF.One),
      (if (includeZero) 1 else 0) -> Gen.const(KleeneF.Zero)
    )

  implicit def arbKleeneF[A](implicit arbA: Arbitrary[A]): Arbitrary[KleeneF[A]] =
    Arbitrary(genKleeneF(arbA.arbitrary, includeZero = true))
}
