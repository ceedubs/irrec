package ceedubs.irrec.regex
package gen

import GreedinessGen.genGreediness

import cats.implicits._
import org.scalacheck.Gen

object QuantifierGen {
  val genOptional: Gen[Quantifier.Optional] = genGreediness.map(Quantifier.Optional(_))
  val genExact: Gen[Quantifier.Exact] = for {
    maxCount <- Gen.size
    count <- Gen.chooseNum(0, maxCount)
  } yield Quantifier.Exact(count)

  val genRange: Gen[Quantifier.Range] = for {
    maxCount <- Gen.size
    lower <- Gen.choose(0, maxCount)
    upper <- Gen.frequency(1 -> None, 5 -> Gen.choose(lower, maxCount).map(_.some))
    g <- genGreediness
  } yield Quantifier.Range(lower, upper, g)

  val genQuantifier: Gen[Quantifier] = Gen.frequency(
    2 -> genOptional,
    1 -> genExact,
    1 -> genRange
  )

  def genCount(q: Quantifier): Gen[Int] = q match {
    case Quantifier.Exact(n) => Gen.const(n)
    case Quantifier.Range(min, max, _) =>
      Gen.sized(maxSize => Gen.choose(min, max.getOrElse(maxSize)))
    case Quantifier.Optional(_) => Gen.choose(0, 1)
  }
}
