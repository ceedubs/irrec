package ceedubs.irrec
package regex

import ceedubs.irrec.regex.ScalacheckSupport._

import cats.implicits._
import qq.droste.{Algebra, CoalgebraM, scheme}
import qq.droste.data.{Mu, CoattrF}
import qq.droste.data.prelude._
import org.scalacheck.{Arbitrary, Gen}, Gen.Choose

object RegexGen {

  def matchingGen[A](m: Match[A], genA: Gen[A])(implicit chooseA: Choose[A]): Gen[A] = m match {
    case Match.Literal(expected) => Gen.const(expected)
    case Match.Wildcard => genA
    case Match.Range(l, r) => chooseA.choose(l, r)
  }

  def kleeneFStreamAlgebra[A]: Algebra[KleeneF, Gen[Stream[A]]] = Algebra{
    case KleeneF.Plus(l, r) => Gen.oneOf(l, r)
    case KleeneF.Times(l, r) => l.flatMap(ls => r.map(rs => ls ++ rs))
    // TODO ceedubs probably need to do something fancier so we don't get large nested structures
    case KleeneF.Star(g) => Gen.sized(maxSize =>
      for {
        size <- Gen.chooseNum(0, maxSize)
        sa <- g
      } yield Stream.fill(size)(sa).flatten
    )
    case KleeneF.Zero => Gen.fail
    case KleeneF.One => Gen.const(Stream.empty)
  }

  def regexMatchingStreamGen[A:Choose](genA: Gen[A]): Algebra[CoattrF[KleeneF, Match[A], ?], Gen[Stream[A]]] =
    Algebra[CoattrF[KleeneF, Match[A], ?], Gen[Stream[A]]]{
      CoattrF.un(_) match {
        case Left(ma) => matchingGen(ma, genA).map(Stream(_))
        case Right(kf) => kleeneFStreamAlgebra(kf)
      }
    }

  def regexMatchingStringGen(r: Regex[Char], genChar: Gen[Char]): Gen[String] =
    r(regexMatchingStreamGen(genChar)).map(_.mkString)

  private def genRangeMatch[A](genA: Gen[A])(implicit orderingA: Ordering[A]): Gen[Match.Range[A]] =
    for {
      a1 <- genA
      a2 <- genA
    } yield if (orderingA.lt(a1, a2)) Match.Range(a1, a2) else Match.Range(a2, a1)

  def genMatch[A](genA: Gen[A])(implicit orderingA: Ordering[A]): Gen[Match[A]] =
    Gen.frequency(
      5 -> genA.map(Match.Literal(_)),
      3 -> genRangeMatch(genA),
      1 -> Gen.const(Match.Wildcard))

  def genRegexCoalgebraM[A:Choose:Ordering](genA: Gen[A], includeZero: Boolean): CoalgebraM[Gen, CoattrF[KleeneF, Match[A], ?], Int] = {
    val leafGen: Gen[CoattrF[KleeneF, Match[A], Int]] =
      Gen.frequency(
        10 -> genMatch[A](genA).map(CoattrF.pure),
        2 -> Gen.const(CoattrF.roll(KleeneF.One)),
        (if (includeZero) 1 else 0) -> Gen.const(CoattrF.roll(KleeneF.Zero)))

    CoalgebraM[Gen, CoattrF[KleeneF, Match[A], ?], Int]((maxSize: Int) =>
      Gen.choose(0, maxSize).flatMap(size =>
        if (size === 0) leafGen
        else {
          val newSize = size - 1
          Gen.frequency(
          10 -> Gen.const(CoattrF.roll(KleeneF.Times(newSize, newSize))),
          5 -> leafGen,
          4 -> Gen.const(CoattrF.roll(KleeneF.Plus(newSize, newSize))),
          2 -> Gen.const(CoattrF.roll(KleeneF.Star(newSize))))
        }
      )
    )
  }

  def genRegex[A:Choose:Ordering](genA: Gen[A], includeZero: Boolean): Gen[Regex[A]] = Gen.sized(maxSize =>
    scheme[Mu].anaM(genRegexCoalgebraM[A](genA, includeZero)).apply(maxSize))

  implicit def arbRegex[A:Choose:Ordering](implicit arbA: Arbitrary[A]): Arbitrary[Regex[A]] =
    Arbitrary(genRegex(arbA.arbitrary, true))
}
