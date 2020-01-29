package ceedubs.irrec
package regex
package gen

import ceedubs.irrec.regex.gen.DietGen.dietMatchingGen
import ceedubs.irrec.regex.gen.ScalacheckSupport._

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
    matchGen: Match[In] => Gen[In]): RegexM[In, ?] ~> λ[a => Gen[Stream[In]]] =
    new (RegexM[In, ?] ~> λ[a => Gen[Stream[In]]]) { outer =>
      def apply[Out](fa: RegexM[In, Out]): Gen[Stream[In]] =
        Regex.fold[In, Match[In], Out, Gen[Stream[In]]](
          eps = _ => Gen.const(Stream.empty),
          fail = () => Gen.fail,
          elem = (m, _) => matchGen(m).map(Stream(_)),
          andThen = λ[λ[i => (RegexM[In, i => Out], RegexM[In, i])] ~> λ[a => Gen[Stream[In]]]](t =>
            outer.apply(t._1).map2(outer.apply(t._2))(_ ++ _)),
          star =
            λ[λ[i => (RegexM[In, i], Greediness, Out, (Out, i) => Out)] ~> λ[a => Gen[Stream[In]]]](
              t => Gen.containerOf[Stream, Stream[In]](outer.apply(t._1)).map(_.flatten)),
          mapped =
            λ[λ[a => (RegexM[In, a], a => Out)] ~> λ[a => Gen[Stream[In]]]](t => outer.apply(t._1)),
          or = alternatives => Gen.oneOf(alternatives.toList).flatMap(apply),
          void = _ => λ[RegexM[In, ?] ~> λ[a => Gen[Stream[In]]]](outer.apply(_))
        )(fa)
    }

  def dietRegexMatchingStreamGen[In: Choose: Discrete: Order, Out](
    available: Diet[In]): RegexM[In, Out] => Gen[Stream[In]] =
    regexMatchingStreamGen[In](dietMatchToGen(available, dietMatchingGen(_))).apply

  def genRegexMatch[In, M, Out](r: Regex[In, M, Out])(
    implicit rc: RegexCandidates[In, M]): Gen[Stream[In]] =
    rc.genMatchingStream(r)
}
