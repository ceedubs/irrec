package ceedubs.irrec
package regex

import DietGen.dietMatchingGen

import cats.implicits._
import cats.Order
import cats.collections.{Diet, Discrete, Range}
import higherkindness.droste.{scheme, Algebra}
import higherkindness.droste.data.CoattrF
import higherkindness.droste.data.prelude._
import org.scalacheck.Gen, Gen.Choose

object RegexMatchGen {

  val byteMatchingGen: Match[Byte] => Gen[Byte] =
    dietMatchToGen(Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)), dietMatchingGen(_))

  val intMatchingGen: Match[Int] => Gen[Int] =
    dietMatchToGen(Diet.fromRange(Range(Int.MinValue, Int.MaxValue)), dietMatchingGen(_))

  val longMatchingGen: Match[Long] => Gen[Long] =
    dietMatchToGen(Diet.fromRange(Range(Long.MinValue, Long.MaxValue)), dietMatchingGen(_))

  final case class Weighted[A](weight: Int, value: A)

  /**
   * A recursive KleeneF structure is roughly an unbalanced binary tree. For a regular expression
   * like `a|b|c|d|e`, if we were to pick each immediate branch of the tree with equal probability,
   * we would end up favoring the characters toward the top of the tree. We work around this by
   * assigning a weight to each node which corresponds to the number of alternations represented by
   * its entire subtree.
   */
  def kleeneFStreamAlgebra[A]: Algebra[KleeneF, Weighted[Gen[Stream[A]]]] = Algebra {
    case KleeneF.Plus(l, r) =>
      Weighted(l.weight + r.weight, Gen.frequency(l.weight -> l.value, r.weight -> r.value))
    case KleeneF.Times(l, r) =>
      Weighted(math.max(l.weight, r.weight), l.value.flatMap(ls => r.value.map(rs => ls ++ rs)))
    case KleeneF.Star(g) =>
      Weighted(g.weight, Gen.containerOf[Stream, Stream[A]](g.value).map(_.flatten))
    case KleeneF.Zero => Weighted(0, Gen.fail)
    case KleeneF.One => Weighted(1, Gen.const(Stream.empty))
  }

  def regexMatchingStreamAlgebra[A](matchGen: Match[A] => Gen[A])
    : Algebra[CoattrF[KleeneF, Match[A], ?], Weighted[Gen[Stream[A]]]] =
    Algebra[CoattrF[KleeneF, Match[A], ?], Weighted[Gen[Stream[A]]]] {
      CoattrF.un(_) match {
        case Left(ma) => Weighted(1, matchGen(ma).map(Stream(_)))
        case Right(kf) => kleeneFStreamAlgebra(kf)
      }
    }

  def dietMatchToGen[A](available: Diet[A], dietToGen: Diet[A] => Gen[A])(
    implicit orderA: Order[A],
    discreteA: Discrete[A]): Match[A] => Gen[A] = _ match {
    case Match.Literal(expected) => Gen.const(expected)
    case Match.Wildcard() => dietToGen(available)
    case Match.MatchSet.Allow(allowed) => dietToGen(allowed)
    case Match.MatchSet.Forbid(forbidden) => dietToGen(available -- forbidden)
  }

  /**
   * Generate a stream that matches the provided regular expression.
   *
   * @param matchToGen how to generate `A` instances for a given `Match[A]`
   */
  def regexMatchingStreamGen[A](matchToGen: Match[A] => Gen[A]): Regex[A] => Gen[Stream[A]] =
    scheme.cata(regexMatchingStreamAlgebra(matchToGen)) andThen (_.value)

  def dietRegexMatchingStreamGen[A: Choose: Discrete: Order](
    available: Diet[A]): Regex[A] => Gen[Stream[A]] =
    regexMatchingStreamGen[A](dietMatchToGen(available, dietMatchingGen(_)))
}
