package ceedubs.irrec
package regex.applicative
// TODO package

import Regex.Regex
import ceedubs.irrec.regex.{Match, RegexGen => RegexGenOld}
import RegexGenOld.{standardByteConfig, standardIntConfig, standardLongConfig}

import cats.implicits._
import cats.Order
import org.scalacheck.{Arbitrary, Cogen, Gen}, Arbitrary.arbitrary

// we could generate regexes that aren't Match-based. It probably makes sense to distinguish between RegexG and Regex here...
object RegexGen {
  val genGreediness: Gen[Greediness] = Gen.oneOf(Greediness.Greedy, Greediness.NonGreedy)

  // TODO add ability to distribute the size across the elements
  //private def genNonEmptyList[A](genA: Gen[A]): Gen[NonEmptyList[A]] = genA.map2(Gen.listOf(genA))((head, tail) => NonEmptyList(head, tail))

  // TODO Should we take a Gen[A] instead of expecting an Arbitrary[A]?
  // TODO incorporate more constructors
  // TODO should the implicit parameters go into the config?
  // TODO Unit version of this that includes Void and Eps
  def genRegex[In:Order:Cogen, Out:Arbitrary](cfg: RegexGenOld.Config[In]): Gen[Regex[In, Out]] = {
    // TODO maybe we don't need the go thing
    Gen.frequency(
      //10 -> go(newSize).map2(go(newSize))(_ *> _),
      //4 -> genNonEmptyList(go(newSize)).map(RE.Or(_)),
      //4 -> Gen.const(CoattrF.roll(KleeneF.Plus(newSize, newSize))),
      //2 -> go(newSize).map2(genGreediness)(_.star(_).void)
      2 -> (
        for {
          m <- cfg.genMatch
          f <- arbitrary[In => Out]
        } yield Regex.mapMatch(m, f)
        ),
      // TODO adjust weights
      2 -> (
        for {
          regexGen <- genRegexWithEv[In](cfg)
          r <- regexGen.evidence.regexGen
          f <- Arbitrary.arbFunction1[regexGen.T, Out](implicitly, regexGen.evidence.cogen).arbitrary
        } yield r.map(f)),
      (if (cfg.includeZero) 1 else 0) -> Gen.const(Regex.fail),
    )
  }

  // TODO should this have Order as well?
  final case class GenAndCogen[A](gen: Gen[A], cogen: Cogen[A])

  object GenAndCogen {
    def of[A](implicit arb: Arbitrary[A], cogen: Cogen[A]): GenAndCogen[A] = GenAndCogen(arb.arbitrary, cogen)
  }

  // TODO ceedubs is this the right path?
  // TODO naming
  // it seems like we actually want a Gen[RE[In, M, Out]] in here instead of a single regex?
  // TODO consider helper function for creating functions?
  final case class RegexWithEv[In, M, Out](regexGen: Gen[RE[In, M, Out]], cogen: Cogen[Out])

  object RegexWithEv {
    def fromRegexGen[In, M, Out](regex: Gen[RE[In, M, Out]])(implicit cogenOut: Cogen[Out]): RegexWithEv[In, M, Out] = RegexWithEv(regex, cogenOut)
  }

  // TODO private?
  val genTypeWithCogen: Gen[TypeWith[Cogen]] = Gen.oneOf(TypeWith(Cogen[Unit]), TypeWith(Cogen[Boolean]), TypeWith(Cogen[Int]), TypeWith(Cogen[Long]), TypeWith(Cogen[Double]), TypeWith(Cogen[String]))

  // TODO private?
  //val genTypeWithGen: Gen[TypeWith[Gen]] = Gen.oneOf(TypeWith(Gen[Unit]), TypeWith(Gen[Boolean]), TypeWith(Gen[Int]), TypeWith(Gen[Long]), TypeWith(Gen[Double]), TypeWith(Gen[String]))

  val genTypeWithGenAndCogen: Gen[TypeWith[GenAndCogen]] = Gen.oneOf(TypeWith(GenAndCogen.of[Unit]), TypeWith(GenAndCogen.of[Boolean]), TypeWith(GenAndCogen.of[Int]), TypeWith(GenAndCogen.of[Long]), TypeWith(GenAndCogen.of[Double]), TypeWith(GenAndCogen.of[String]))

  // TODO
  private[irrec] implicit def ambGenConversion1[A](a: A): Gen[A] = sys.error(s"🍩 use implicit Gen conversion: $a")
  private[irrec] implicit def ambGenConversion2[A](a: A): Gen[A] = sys.error(s"🍩 use implicit Gen conversion: $a")

  // TODO how to prevent duplicating logic between this and the other?
  // Seems like we should be able to pass in an explicit TypeWith?
  def genRegexWithEv[In: Cogen : Order](cfg: RegexGenOld.Config[In]): Gen[TypeWith[RegexWithEv[In, Match[In], ?]]] = {
    val leafGen: Gen[TypeWith[RegexWithEv[In, Match[In], ?]]] = Gen.frequency(
      (if (cfg.includeOne) 2 else 0) -> Gen.const(TypeWith(RegexWithEv.fromRegexGen(Gen.const(Regex.empty[In, Match[In]])))),
      (if (cfg.includeZero) 1 else 0) -> genTypeWithCogen.map(t => TypeWith(RegexWithEv(Gen.const(Regex.fail[t.T]), t.evidence))),
      10 -> {
        genTypeWithGenAndCogen.map{ t =>
          val genR = for {
            m <- cfg.genMatch
            f <- {
              implicit val genT = Arbitrary(t.evidence.gen)
              Arbitrary.arbFunction1[In, t.T].arbitrary
            }
          } yield Regex.mapMatch(m, f)
          TypeWith(RegexWithEv(genR, t.evidence.cogen))
        }
      }
    )
    // TODO
    def go(maxSize: Int): Gen[TypeWith[RegexWithEv[In, Match[In], ?]]] =
      Gen.choose(1, maxSize).flatMap(size =>
        if (size <= 1) leafGen
        else {
          // TODO it doesn't really make sense to constrain both branches to have the same size, does it?
          // TODO other type that aren't covered
          Gen.frequency(
            //10 -> go(newSize).map2(go(newSize))(_ *> _),
            // TODO handle sizing of nested gens appropriately
            10 -> {
              for {
                rI <- go(size)
                outType <- genTypeWithGenAndCogen
              } yield {
                val genR: Gen[RE[In, Match[In], outType.T]] = for {
                ri <- rI.evidence.regexGen
                rf <- Gen.resize(maxSize + 1 - size, genRegex[In, rI.T => outType.T](cfg)(implicitly, implicitly, Arbitrary.arbFunction1(Arbitrary(outType.evidence.gen), rI.evidence.cogen)))
                } yield RE.AndThen(rf, ri)
                TypeWith(RegexWithEv(genR, outType.evidence.cogen))
              }
            },
            // TODO should we include leafGen in here? Probably not since we already checked the size
            //1 -> leafGen,
            // TODO clean up and modularize
            // TODO maybe cleaner to create a helper method based on Gen instead of arb?
            // Void
            1 -> go(size - 1).map{ r =>
              TypeWith(RegexWithEv.fromRegexGen(r.evidence.regexGen.map(_.void)))
            },
            // FMap
            1 -> go(size - 1).flatMap { r =>
              implicit val cogenT = r.evidence.cogen
              genTypeWithGenAndCogen.map{ outType =>
                implicit val arbOut = Arbitrary(outType.evidence.gen)
                val genR = r.evidence.regexGen.flatMap{ regex =>
                  arbitrary[r.T => outType.T].map(f => RE.FMap(regex, f))
                }
                TypeWith(RegexWithEv(genR, outType.evidence.cogen))
              }
            }
          )
        })
    Gen.sized(go(_))
  }

  // TODO not just Unit
  val genByteRegex: Gen[Regex[Byte, Unit]] = genRegex(standardByteConfig)

  implicit val arbByteRegex: Arbitrary[Regex[Byte, Unit]] = Arbitrary(genByteRegex)

  val genIntRegex: Gen[Regex[Int, Unit]] = genRegex(standardIntConfig)

  implicit val arbIntRegex: Arbitrary[Regex[Int, Unit]] = Arbitrary(genIntRegex)

  val genLongRegex: Gen[Regex[Long, Unit]] = genRegex(standardLongConfig)

  implicit val arbLongRegex: Arbitrary[Regex[Long, Unit]] = Arbitrary(genLongRegex)
}
