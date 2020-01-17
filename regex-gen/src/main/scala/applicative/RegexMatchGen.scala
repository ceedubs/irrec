package ceedubs.irrec
package regex

import Regex.Regex
import ceedubs.irrec.regex.DietGen.dietMatchingGen
import ceedubs.irrec.regex.ScalacheckSupport._

import cats.{~>, Order}
import org.scalacheck.Gen, Gen.Choose
import cats.collections.{Diet, Discrete, Range}
import cats.implicits._

object RegexMatchGen {
  def dietMatchToGen[A](available: Diet[A], dietToGen: Diet[A] => Gen[A])(
    implicit orderA: Order[A],
    discreteA: Discrete[A]): Match[A] => Gen[A] = _ match {
    case Match.Literal(expected) => Gen.const(expected)
    case Match.Wildcard() => dietToGen(available)
    case Match.MatchSet.Allow(allowed) => dietToGen(allowed)
    case Match.MatchSet.Forbid(forbidden) => dietToGen(available -- forbidden)
  }

  val byteMatchingGen: Match[Byte] => Gen[Byte] =
    dietMatchToGen(Diet.fromRange(Range(Byte.MinValue, Byte.MaxValue)), dietMatchingGen(_))

  val intMatchingGen: Match[Int] => Gen[Int] =
    dietMatchToGen(Diet.fromRange(Range(Int.MinValue, Int.MaxValue)), dietMatchingGen(_))

  val longMatchingGen: Match[Long] => Gen[Long] =
    dietMatchToGen(Diet.fromRange(Range(Long.MinValue, Long.MaxValue)), dietMatchingGen(_))

  def regexMatchingStreamGen[In](
    matchGen: Match[In] => Gen[In]): Regex[In, ?] ~> λ[a => Gen[Stream[In]]] =
    new (Regex[In, ?] ~> λ[a => Gen[Stream[In]]]) { outer =>
      // TODO think about whether kind-projector literals are actually making this any cleaner
      def apply[Out](fa: RE[In, Match[In], Out]): Gen[Stream[In]] =
        RE.fold[In, Match[In], Out, Gen[Stream[In]]](
          eps = _ => Gen.const(Stream.empty),
          fail = () => Gen.fail,
          elem = (m, _) => matchGen(m).map(Stream(_)),
          andThen = λ[
            λ[i => (RE[In, Match[In], i => Out], RE[In, Match[In], i])] ~> λ[a => Gen[Stream[In]]]](
            t => outer.apply(t._1).map2(outer.apply(t._2))(_ ++ _)),
          star = λ[λ[i => (RE[In, Match[In], i], Greediness, Out, (Out, i) => Out)] ~> λ[
            a => Gen[Stream[In]]]](t =>
            Gen.containerOf[Stream, Stream[In]](outer.apply(t._1)).map(_.flatten)),
          mapped = λ[λ[a => (RE[In, Match[In], a], a => Out)] ~> λ[a => Gen[Stream[In]]]](t =>
            outer.apply(t._1)),
          or = alternatives => Gen.oneOf(alternatives.toList).flatMap(apply),
          void = _ => λ[RE[In, Match[In], ?] ~> λ[a => Gen[Stream[In]]]](outer.apply(_))
        )(fa)
    }

  def dietRegexMatchingStreamGen[In: Choose: Discrete: Order, Out](
    available: Diet[In]): Regex[In, Out] => Gen[Stream[In]] =
    regexMatchingStreamGen[In](dietMatchToGen(available, dietMatchingGen(_))).apply
}
