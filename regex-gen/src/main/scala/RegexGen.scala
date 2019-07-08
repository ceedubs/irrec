package ceedubs.irrec
package regex

import ceedubs.irrec.regex.ScalacheckSupport._
import ceedubs.irrec.regex.Match.MatchSet
import DietGen._

import cats.Order
import cats.collections.{Diet, Discrete, Range}
import cats.implicits._
import qq.droste.{scheme, CoalgebraM}
import qq.droste.data.CoattrF
import qq.droste.data.prelude._
import org.scalacheck.{Arbitrary, Gen}, Gen.Choose

object RegexGen {

  /**
   * Configuration for generating regular expressions.
   */
  final class Config[A](
    val genA: Gen[A],
    val genMatch: Gen[Match[A]],
    val includeZero: Boolean,
    val includeOne: Boolean)

  object Config {

    def fromDiscreteDiet[A: Choose: Discrete: Order](available: Diet[A]): Config[A] = {
      val genA = dietMatchingGen(available)
      new Config(
        genA = genA,
        genMatch = genMatch(genA, genNonEmptySubDiet(available, _ => 1)),
        includeZero = false,
        includeOne = false
      )
    }
  }

  def genMatch[A: Discrete: Order](genA: Gen[A], genDietA: Gen[Diet[A]]): Gen[Match[A]] =
    Gen.frequency(
      9 -> genA.map(Match.lit(_)),
      3 -> genDietA.map(MatchSet.allow(_)),
      2 -> genDietA.map(MatchSet.forbid(_)),
      1 -> Gen.const(Match.wildcard)
    )

  def genRegexCoalgebraM[A](cfg: Config[A]): CoalgebraM[Gen, CoattrF[KleeneF, Match[A], ?], Int] = {
    val leafGen: Gen[CoattrF[KleeneF, Match[A], Int]] =
      Gen.frequency(
        10 -> cfg.genMatch.map(CoattrF.pure),
        (if (cfg.includeOne) 2 else 0) -> Gen.const(CoattrF.roll(KleeneF.One)),
        (if (cfg.includeZero) 1 else 0) -> Gen.const(CoattrF.roll(KleeneF.Zero))
      )

    CoalgebraM[Gen, CoattrF[KleeneF, Match[A], ?], Int](
      (maxSize: Int) =>
        Gen
          .choose(0, maxSize)
          .flatMap(size =>
            if (size === 0) leafGen
            else {
              val newSize = size - 1
              Gen.frequency(
                10 -> Gen.const(CoattrF.roll(KleeneF.Times(newSize, newSize))),
                5 -> leafGen,
                4 -> Gen.const(CoattrF.roll(KleeneF.Plus(newSize, newSize))),
                2 -> Gen.const(CoattrF.roll(KleeneF.Star(newSize)))
              )
            }))
  }

  def genRegex[A](cfg: Config[A]): Gen[Regex[A]] =
    Gen.sized(maxSize => scheme.anaM(genRegexCoalgebraM[A](cfg)).apply(maxSize))

  val standardByteConfig: Config[Byte] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)))

  val standardIntConfig: Config[Int] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Int.MinValue, Int.MaxValue)))

  val standardLongConfig: Config[Long] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Long.MinValue, Long.MaxValue)))

  val genByteRegex: Gen[Regex[Byte]] = genRegex(standardByteConfig)

  implicit val arbByteRegex: Arbitrary[Regex[Byte]] = Arbitrary(genByteRegex)

  val genIntRegex: Gen[Regex[Int]] = genRegex(standardIntConfig)

  implicit val arbIntRegex: Arbitrary[Regex[Int]] = Arbitrary(genIntRegex)

  val genLongRegex: Gen[Regex[Long]] = genRegex(standardLongConfig)

  implicit val arbLongRegex: Arbitrary[Regex[Long]] = Arbitrary(genLongRegex)
}
