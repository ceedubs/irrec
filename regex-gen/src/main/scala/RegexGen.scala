package ceedubs.irrec
package regex

import ceedubs.irrec.regex.ScalacheckSupport._

import cats.Order
import cats.collections.{Diet, Discrete, Range}
import cats.implicits._
import qq.droste.{scheme, CoalgebraM}
import qq.droste.data.CoattrF
import qq.droste.data.prelude._
import org.scalacheck.{Arbitrary, Gen}, Gen.Choose, Arbitrary.arbitrary

object RegexGen {

  // TODO ceedubs should this really be a case class? Equality doesn't make sense.
  /**
   * Configuration for generating regular expressions.
   */
  final case class Config[A](
    gen: Gen[A],
    genDiet: Gen[Diet[A]],
    discreteA: Discrete[A],
    orderA: Order[A],
    includeZero: Boolean,
    includeOne: Boolean)

  // TODO ceedubs this tries to use things from the outside scope forcing them to be lazy. Fix.
  object Config {
    // TODO ceedubs this is pretty busted
    //def fromDiscreteRange[A: Choose: Discrete: Order](range: Range[A]): Config[A] =
    //  Config(
    //    gen = Gen.choose(range.start, range.end),
    //    genDiet = genNonEmptyDiet(Gen.const(range)),
    //    includeZero = false,
    //    includeOne = false)

    // TODO ceedubs does this need to be integral any more?
    def fromIntegralDiet[A:Choose:Integral](available: Diet[A]): Config[A] = {
      // TODO ceedubs is this crazy?
      // TODO ceedubs do we need Integral?
      implicit val orderA: Order[A] = Order.fromOrdering(implicitly[Integral[A]])
      Config(
        gen = RegexMatchGen.dietMatchingGen(available),
        genDiet = genNonEmptySubDiet(available),
        discreteA = implicitly,
        orderA = implicitly,
        includeZero = false,
        // TODO ceedubs should this be one?
        includeOne = false)
    }

  }

  // TODO ceedubs
  //def matchingGen[A](m: Match[A], genA: Gen[A])(
  //  implicit chooseA: Choose[A],
  //  orderA: Order[A]): Gen[A] = m match {
  //  case Match.Literal(expected) => Gen.const(expected)
  //  case Match.Wildcard => genA
  //  case Match.Range(l, r) => chooseA.choose(l, r)
  //  case m @ Match.NoneOf(_) => genA.filter(m.matches(_))
  //}

  def genSubrange[A: Choose: Order](range: Range[A]): Gen[Range[A]] =
    for {
      x <- Gen.choose(range.start, range.end)
      y <- Gen.choose(range.start, range.end)
    } yield if (x < y) Range(x, y) else Range(y, x)

  // TODO ceedubs is this needed?
  def genNonEmptySubDiet[A: Choose: Discrete: Order](diet: Diet[A]): Gen[Diet[A]] = {
    val ranges = diet.foldLeftRange(List.empty[Range[A]])((ranges, range) => range :: ranges)
    val newRanges: Gen[List[Range[A]]] = for {
      childRangeCount <- Gen.choose(1, ranges.size)
      filteredRanges <- Gen.pick(childRangeCount, ranges)
      newSubranges <- Gen.sequence[List[Range[A]], Range[A]](filteredRanges.map(genSubrange(_)))
    } yield newSubranges
    // TODO ceedubs use Diet.fromRange when it's available
    newRanges.map(_.foldMap(Diet.fromRange _))
  }

  // TODO ceedubs document
  // TODO ceedubs needed?
  //def rangeWeightedRegexMatchingStreamGen[A: Choose:Order:Integral](available: Diet[A]): Regex[A] => Gen[Stream[A]] =
  //  scheme.cata(
  //    regexMatchingStreamAlgebra[A](
  //      available,
  //      weight = rangeLength(_))
  //  ) andThen (_.value)

  // TODO ceedubs is this needed?
  def genRange[A](genA: Gen[A])(implicit orderingA: Ordering[A]): Gen[Range[A]] =
    for {
      a1 <- genA
      a2 <- genA
    } yield if (orderingA.lt(a1, a2)) Range(a1, a2) else Range(a2, a1)

  // TODO ceedubs is this needed?
  // TODO ceedubs this is being used the wrong way I think. It's being passed in with a Gen.const of
  // a range and that range is never split up.
  //def genNonEmptyDiet[A: Order: Discrete](genRange: Gen[Range[A]]): Gen[Diet[A]] =
  //  Gen.nonEmptyListOf(genRange).map(_.foldLeft(Diet.empty[A])(_ addRange _))

  def genMatch[A:Discrete:Order](genA: Gen[A], genDietA: Gen[Diet[A]]): Gen[Match[A]] =
    Gen.frequency(
      9 -> genA.map(Match.lit(_)),
      // TODO ceedubs should this be better?
      // clean up
      // TODO ceedubs don't generate empty matchers
      3 -> {
        for {
          pos <- Gen.option(genDietA)
          neg <- genDietA
          op <- arbitrary[Boolean]
        } yield {
          val negM = Match.MatchSet.forbid(neg)
          pos.fold(negM){ pos =>
            val posM = Match.MatchSet.allow(pos)
            val attempt = if (op) posM union negM else posM intersect negM
            // TODO ceedubs this is pretty hacky. Does it work?
            attempt.positive.fold(negM){ pos =>
              // TODO ceedubs is posM right here?
              if ((pos -- neg).isEmpty) posM else attempt
            }
          }
        }
      },
      1 -> Gen.const(Match.wildcard)
    )

  def genRegexCoalgebraM[A](cfg: Config[A]): CoalgebraM[Gen, CoattrF[KleeneF, Match[A], ?], Int] = {
    val leafGen: Gen[CoattrF[KleeneF, Match[A], Int]] =
      Gen.frequency(
        10 -> genMatch[A](cfg.gen, cfg.genDiet)(cfg.discreteA, cfg.orderA).map(CoattrF.pure),
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

  // TODO ceedubs do we need both a `Gen[A]` and a `Gen[Diet[A]]`?
  // Should we just be taking a `Diet[A]` as an arg?
  def genRegex[A](cfg: Config[A]): Gen[Regex[A]] =
    Gen.sized(maxSize => scheme.anaM(genRegexCoalgebraM[A](cfg)).apply(maxSize))

  // TODO ceedubs we have better ways of generating these
  //def arbRegex[A](
  //  implicit arbA: Arbitrary[A],
  //  discreteA: Discrete[A],
  //  orderA: Order[A]): Arbitrary[Regex[A]] =
  //  Arbitrary(
  //    genRegex(
  //      arbA.arbitrary,
  //      genDiet(genRange(arbA.arbitrary)),
  //      includeZero = true,
  //      includeOne = true))

  // TODO ceedubs where to put these? lazy? Why are things broken?
  // TODO ceedubs these still depend on Integral?
  lazy val standardByteConfig: Config[Byte] = Config.fromIntegralDiet(Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)))
  lazy val standardIntConfig: Config[Int] = Config.fromIntegralDiet(Diet.fromRange(Range(Int.MinValue, Int.MaxValue)))
  lazy val standardLongConfig: Config[Long] = Config.fromIntegralDiet(Diet.fromRange(Range(Long.MinValue, Long.MaxValue)))

  val genByteRegex: Gen[Regex[Byte]] = genRegex(standardByteConfig)

  implicit val arbByteRegex: Arbitrary[Regex[Byte]] = Arbitrary(genByteRegex)

  val genIntRegex: Gen[Regex[Int]] = genRegex(standardIntConfig)

  implicit val arbIntRegex: Arbitrary[Regex[Int]] = Arbitrary(genIntRegex)

  val genLongRegex: Gen[Regex[Long]] = genRegex(standardLongConfig)

  implicit val arbLongRegex: Arbitrary[Regex[Long]] = Arbitrary(genLongRegex)
}
