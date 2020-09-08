package ceedubs.irrec
package regex
package gen

import org.scalacheck.Gen, Gen.Choose
import cats.kernel.Order
import cats.collections.{Diet, Discrete, Range}
import cats.implicits._

object DietGen {
  def genNonEmptySubDiet[A: Choose: Discrete: Order](
    diet: Diet[A],
    weight: Range[A] => Int): Gen[Diet[A]] = {
    val genA = weightedDietMatchingGen(diet, weight)
    Gen.nonEmptyListOf(genA).map(_.foldMap(Diet.one(_)))
  }

  def weightedDietMatchingGen[A](diet: Diet[A], weight: Range[A] => Int)(implicit
    chooseA: Choose[A]): Gen[A] =
    genWeightedDietRange(diet, weight).flatMap(range => Gen.choose(range.start, range.end))

  def dietMatchingGen[A: Choose](diet: Diet[A]): Gen[A] = weightedDietMatchingGen[A](diet, _ => 1)

  def genWeightedDietRange[A](diet: Diet[A], weight: Range[A] => Int): Gen[Range[A]] = {
    val freqs = diet.foldLeftRange(List.empty[(Int, Gen[Range[A]])]) { case (gens, range) =>
      (weight(range), Gen.const(range)) :: gens
    }
    Gen.frequency(freqs: _*)
  }

  def genRange[A](genA: Gen[A])(implicit orderingA: Order[A]): Gen[Range[A]] =
    for {
      a1 <- genA
      a2 <- genA
    } yield if (orderingA.lt(a1, a2)) Range(a1, a2) else Range(a2, a1)

  def genDiscreteDiet[A: Discrete: Order](genA: Gen[A]): Gen[Diet[A]] =
    for {
      ranges <- Gen.listOf(genRange(genA))
      individualValues <- Gen.listOf(genA)
    } yield ranges.foldMap(Diet.fromRange) ++ individualValues.foldMap(Diet.one)
}
