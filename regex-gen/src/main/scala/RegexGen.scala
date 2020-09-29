package ceedubs.irrec
package regex
package gen

import DietGen._
import ceedubs.irrec.regex.Match.MatchSet
import ceedubs.irrec.regex.gen.ScalacheckSupport._
import GreedinessGen.genGreediness

import cats.implicits._
import cats.Order
import cats.data.NonEmptyList
import cats.collections.{Diet, Discrete, Range}
import org.scalacheck.{Arbitrary, Cogen, Gen}, Arbitrary.arbitrary, Gen.Choose

// we could generate regexes that aren't Match-based. It probably makes sense to distinguish between RegexG and Regex here...
object RegexGen {
  import Support._

  /**
   * Configuration for generating regular expressions with input type `In`.
   */
  final case class Config[In](
    gen: Gen[In],
    cogen: Cogen[In],
    order: Order[In],
    genMatch: Gen[Match[In]],
    includeFail: Boolean,
    includeEps: Boolean,
    includeMapFilterNone: Boolean)

  object Config {
    def fromDiscreteDiet[A: Choose: Cogen: Discrete: Order](available: Diet[A]): Config[A] = {
      val genIn = dietMatchingGen(available)
      Config[A](
        gen = genIn,
        cogen = implicitly,
        order = implicitly,
        genMatch = RegexGen.genMatch(genIn, genNonEmptySubDiet(available, _ => 1)),
        includeFail = false,
        includeEps = false,
        includeMapFilterNone = false
      )
    }
  }

  def genMatch[A: Discrete: Order](genA: Gen[A], genDietA: Gen[Diet[A]]): Gen[Match[A]] =
    Gen.frequency(
      16 -> genA.map(Match.lit(_)),
      3 -> genDietA.map(MatchSet.allow(_)),
      2 -> genDietA.map(MatchSet.forbid(_)),
      1 -> Gen.const(Match.wildcard)
    )

  val standardByteConfig: Config[Byte] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)))

  val standardIntConfig: Config[Int] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Int.MinValue, Int.MaxValue)))

  val standardLongConfig: Config[Long] =
    Config.fromDiscreteDiet(Diet.fromRange(Range(Long.MinValue, Long.MaxValue)))

  implicit val arbGreendiness: Arbitrary[Greediness] = Arbitrary(genGreediness)

  // TODO Should we take a Gen[Out] instead of expecting an Arbitrary[Out]?
  // TODO should this be public?
  private def genRegexWithDepth[In, Out: Arbitrary: Cogen](
    cfg: Config[In],
    depth: Int): Gen[RegexM[In, Out]] = {
    implicit val cogenIn = cfg.cogen
    implicit val orderIn = cfg.order
    if (depth <= 1)
      Gen.frequency(
        // Elem
        9 -> (
          for {
            m <- cfg.genMatch
            f <- arbitrary[In => Out]
          } yield combinator.mapMatch(m, f)
        ),
        // Fail
        (if (cfg.includeFail) 1 else 0) -> Gen.const(combinator.fail)
      )
    else
      Gen.frequency(
        // AndThen
        20 -> (
          for {
            rIDepth <- Gen.choose(1, depth - 1)
            rIGen <- genGenRegexWithEv[In](cfg).apply(rIDepth)
            rI <- rIGen.evidence.regexGen
            rf <- {
              implicit val arbI = Arbitrary(rIGen.evidence.genOut)
              implicit val cogenI = rIGen.evidence.cogenOut
              genRegexWithDepth[In, rIGen.T => Out](cfg, depth - rIDepth)
            }
          } yield combinator.andThen(rf, rI)
        ),
        // FMap
        2 -> (for {
          regexGen <- genGenRegexWithEv[In](cfg).apply(depth - 1)
          r <- regexGen.evidence.regexGen
          f <- {
            implicit val cogenOut = regexGen.evidence.cogenOut
            arbitrary[regexGen.T => Out]
          }
        } yield r.map(f)),
        // MapFilter
        1 -> (for {
          regexGen <- genGenRegexWithEv[In](cfg).apply(depth - 1)
          r <- regexGen.evidence.regexGen
          f <- {
            implicit val cogenOut = regexGen.evidence.cogenOut
            if (cfg.includeMapFilterNone) arbitrary[regexGen.T => Option[Out]]
            else arbitrary[regexGen.T => Out].map(_.andThen(_.some))
          }
        } yield r.mapFilter(f)),
        // Or
        3 -> (
          for {
            rCount <- Gen.chooseNum(1, depth - 1)
            depths <- distributeSumNel(entryCount = rCount, extra = depth - 1 - rCount)
            nel <- depths.traverse(depth => genRegexWithDepth[In, Out](cfg, depth))
          } yield Regex.Or[In, Match[In], Out](nel)
        ),
        // Repeat
        3 -> (
          for {
            maxCount <- Gen.choose(1, math.min(depth - 1, 5))
            rIGen <- genGenRegexWithEv[In](cfg).apply(depth - maxCount)
            rI <- rIGen.evidence.regexGen
            quantifier <- Gen.resize(maxCount, QuantifierGen.genQuantifier)
            z <- arbitrary[Out]
            fold <- {
              implicit val iCogen = rIGen.evidence.cogenOut
              arbitrary[(Out, rIGen.T) => Out]
            }
          } yield rI.quantifyFold(quantifier, z)(fold)
        ),
        // Star
        2 -> (
          for {
            rIGen <- genGenRegexWithEv[In](cfg).apply(depth - 1)
            rI <- rIGen.evidence.regexGen
            g <- arbitrary[Greediness]
            z <- arbitrary[Out]
            fold <- {
              implicit val iCogen = rIGen.evidence.cogenOut
              arbitrary[(Out, rIGen.T) => Out]
            }
          } yield combinator.starFold(rI, g, z)(fold)
        )
      )
  }

  def genRegex[In, Out: Arbitrary: Cogen](cfg: Config[In]): Gen[RegexM[In, Out]] =
    Gen.sized(maxSize =>
      Gen.choose(1, math.max(maxSize, 1)).flatMap(depth => genRegexWithDepth[In, Out](cfg, depth)))

  /**
   * Generates regular expressions with existential `Out` types.
   *
   * The returned function takes an `Int` that indicates the desired "depth" of the regex.
   */
  private def genGenRegexWithEv[In](
    cfg: Config[In]): Int => Gen[TypeWith[GenRegexWithEv[In, Match[In], ?]]] = {
    val leafGen: Gen[TypeWith[GenRegexWithEv[In, Match[In], ?]]] = Gen.frequency(
      9 -> genTypeWithGenAndCogen.map { outType =>
        implicit val arbOut = Arbitrary(outType.evidence.gen)
        implicit val cogenOut = outType.evidence.cogen
        TypeWith(GenRegexWithEv.fromRegexGen(genRegexWithDepth[In, outType.T](cfg, 1)))
      },
      // Eps
      (if (cfg.includeEps) 2 else 0) -> Gen.const(
        TypeWith(GenRegexWithEv.fromRegexGen(Gen.const(combinator.empty[In, Match[In]]))))
    )
    def go(depth: Int): Gen[TypeWith[GenRegexWithEv[In, Match[In], ?]]] =
      if (depth <= 1) leafGen
      else
        Gen.frequency(
          // Void
          1 -> go(depth - 1).map { r =>
            TypeWith(GenRegexWithEv.fromRegexGen(r.evidence.regexGen.map(_.void)))
          },
          9 -> genTypeWithGenAndCogen.map { outType =>
            implicit val arbOut = Arbitrary(outType.evidence.gen)
            implicit val cogenOut = outType.evidence.cogen
            TypeWith(GenRegexWithEv.fromRegexGen(genRegexWithDepth[In, outType.T](cfg, depth)))
          }
        )
    go(_)
  }

  def genByteRegex[Out: Arbitrary: Cogen]: Gen[RegexM[Byte, Out]] = genRegex(standardByteConfig)

  implicit def arbByteRegex[Out: Arbitrary: Cogen]: Arbitrary[RegexM[Byte, Out]] =
    Arbitrary(genByteRegex)

  def genIntRegex[Out: Arbitrary: Cogen]: Gen[RegexM[Int, Out]] = genRegex(standardIntConfig)

  implicit def arbIntRegex[Out: Arbitrary: Cogen]: Arbitrary[RegexM[Int, Out]] =
    Arbitrary(genIntRegex)

  def genLongRegex[Out: Arbitrary: Cogen]: Gen[RegexM[Long, Out]] = genRegex(standardLongConfig)

  implicit def arbLongRegex[Out: Arbitrary: Cogen]: Arbitrary[RegexM[Long, Out]] =
    Arbitrary(genLongRegex)

  def genCharRegex[Out: Arbitrary: Cogen]: Gen[RegexC[Out]] =
    CharRegexGen.genStandardCharRegex

  implicit def arbCharRegex[Out: Arbitrary: Cogen]: Arbitrary[RegexC[Out]] =
    Arbitrary(genCharRegex)

  implicit val arbQuantifier: Arbitrary[Quantifier] = Arbitrary(
    Gen.resize(5, QuantifierGen.genQuantifier))

  private[irrec] object Support {
    final case class GenAndCogen[A](gen: Gen[A], cogen: Cogen[A])

    object GenAndCogen {
      def of[A](implicit arb: Arbitrary[A], cogen: Cogen[A]): GenAndCogen[A] =
        GenAndCogen(arb.arbitrary, cogen)
    }

    final case class GenRegexWithEv[In, M, Out](
      regexGen: Gen[Regex[In, M, Out]],
      genOut: Gen[Out],
      cogenOut: Cogen[Out])

    object GenRegexWithEv {
      def fromRegexGen[In, M, Out](regex: Gen[Regex[In, M, Out]])(implicit
        arbOut: Arbitrary[Out],
        cogenOut: Cogen[Out]): GenRegexWithEv[In, M, Out] =
        GenRegexWithEv(regex, arbOut.arbitrary, cogenOut)
    }

    val genTypeWithGenAndCogen: Gen[TypeWith[GenAndCogen]] = Gen.oneOf(
      TypeWith(GenAndCogen.of[Unit]),
      TypeWith(GenAndCogen.of[Boolean]),
      TypeWith(GenAndCogen.of[Int]),
      TypeWith(GenAndCogen.of[Long]),
      TypeWith(GenAndCogen.of[Double]),
      TypeWith(GenAndCogen.of[String])
    )

    /**
     * Create a NonEmptyList[Int] of size `entryCount` that sums to entryCount + extra. Every item in the list will be at least 1.
     *
     * This can be useful when you want to build a composite structure of some size and want to randomly distribute the size across `entryCount` pieces.
     */
    def distributeSumNel(entryCount: Int, extra: Int): Gen[NonEmptyList[Int]] =
      if (entryCount < 1)
        Gen.fail
      else
        Gen.listOfN(extra, Gen.choose(0, entryCount - 1)).map { indices =>
          val sizes = Array.fill(entryCount)(1)
          indices.foreach(i => sizes(i) += 1)
          NonEmptyList.fromListUnsafe(sizes.toList)
        }

    // Scalacheck includes an implicit converstion A => Gen[A], and it can cause hard-to-spot bugs
    implicit private[irrec] def ambGenConversion1[A](a: A): Gen[A] =
      sys.error(s"🍩 use implicit Gen conversion: $a")
    implicit private[irrec] def ambGenConversion2[A](a: A): Gen[A] =
      sys.error(s"🍩 use implicit Gen conversion: $a")
  }
}
