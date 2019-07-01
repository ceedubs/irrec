package ceedubs.irrec
package regex

import cats.implicits._
import cats.Order
import cats.collections.{Diet, Discrete, Range}
import qq.droste.{scheme, Algebra}
import qq.droste.data.CoattrF
import qq.droste.data.prelude._
import org.scalacheck.Gen, Gen.Choose

object RegexMatchGen {
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
    case Match.MatchSet(pos, neg) =>
      val allowed = pos.getOrElse(available)
      dietToGen(allowed -- neg)
  }

  // TODO ceedus this is the main generator. Need to make it easier to figure out what's what.
  def regexMatchingStreamGen[A](matchToGen: Match[A] => Gen[A]): Regex[A] => Gen[Stream[A]] =
    scheme.cata(regexMatchingStreamAlgebra(matchToGen)) andThen (_.value)

  // TODO ceedubs does that mean that the whole integral thing doesn't even make sense to use?
  // It provides questionable value since we might not want large ranges to dominate.
  //def integralDietMatchingGen[A:Choose:Integral](diet: Diet[A]): Gen[A] =
  //  // TODO ceedubs this breaks for long ranges that don't fit into Int
  //  //weightedDietMatchingGen[A](diet = diet, weight = rangeLength(_))
  //  weightedDietMatchingGen[A](diet = diet, weight = _ => 1)

  // TODO ceedubs _ => 1 hard-coded in lots of places.
  def dietRegexMatchingStreamGen[A:Choose:Discrete:Order](available: Diet[A]): Regex[A] => Gen[Stream[A]] =
    regexMatchingStreamGen[A](dietMatchToGen(available, dietMatchingGen(_)))

  // TODO ceedubs is this needed?
  // TODO ceedubs how bad is default param?
  def weightedDietMatchingGen[A](diet: Diet[A], weight: Range[A] => Int)(
    implicit chooseA: Choose[A]): Gen[A] =
    genWeightedDietRange(diet, weight).flatMap { range =>
      Gen.choose(range.start, range.end)
    }

  def dietMatchingGen[A:Choose](diet: Diet[A]): Gen[A] = weightedDietMatchingGen[A](diet, _ => 1)

  // TODO ceedubs is this needed?
  def genWeightedDietRange[A](diet: Diet[A], weight: Range[A] => Int): Gen[Range[A]] = {
    val freqs = diet.foldLeftRange(List.empty[(Int, Gen[Range[A]])]) {
      case (gens, range) =>
        (weight(range), Gen.const(range)) :: gens
    }
    Gen.frequency(freqs: _*)
  }

  val byteMatchingGen: Match[Byte] => Gen[Byte] = dietMatchToGen(
    Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)),
    dietMatchingGen(_))

  val intMatchingGen: Match[Int] => Gen[Int] = dietMatchToGen(
    Diet.fromRange(Range(Int.MinValue, Int.MaxValue)),
    dietMatchingGen(_))

  val longMatchingGen: Match[Long] => Gen[Long] = dietMatchToGen(
    Diet.fromRange(Range(Long.MinValue, Long.MaxValue)),
    dietMatchingGen(_))
}
