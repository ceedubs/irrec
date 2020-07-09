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
  def dietMatchToGen[A](available: Diet[A], dietToGen: Diet[A] => Gen[A])(implicit
    orderA: Order[A],
    discreteA: Discrete[A]): Match[A] => Gen[A] =
    _ match {
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

  def regexMatchingStreamGen[In, M](
    matchGen: M => Gen[In]): Regex[In, M, ?] ~> λ[a => Gen[Stream[In]]] =
    new (Regex[In, M, ?] ~> λ[a => Gen[Stream[In]]]) { outer =>
      def apply[Out](fa: Regex[In, M, Out]): Gen[Stream[In]] =
        Regex.fold[In, M, Out, Gen[Stream[In]]](
          eps = _ => Gen.const(Stream.empty),
          fail = () => Gen.fail,
          elem = (m, _) => matchGen(m).map(Stream(_)),
          andThen =
            λ[λ[i => (Regex[In, M, i => Out], Regex[In, M, i])] ~> λ[a => Gen[Stream[In]]]](t =>
              outer.apply(t._1).map2(outer.apply(t._2))(_ ++ _)),
          star = λ[
            λ[i => (Regex[In, M, i], Greediness, Out, (Out, i) => Out)] ~> λ[a => Gen[Stream[In]]]](
            t => Gen.containerOf[Stream, Stream[In]](outer.apply(t._1)).map(_.flatten)),
          repeat = λ[
            λ[i => (Regex[In, M, i], Quantifier, Out, (Out, i) => Out)] ~> λ[a => Gen[Stream[In]]]](
            t =>
              for {
                count <- QuantifierGen.genCount(t._2)
                nestedStream <- Gen.containerOfN[Stream, Stream[In]](count, outer.apply(t._1))
              } yield nestedStream.flatten),
          mapped = λ[λ[a => (Regex[In, M, a], a => Out)] ~> λ[a => Gen[Stream[In]]]](t =>
            outer.apply(t._1)),
          or = alternatives => Gen.oneOf(alternatives.toList).flatMap(apply),
          void = _ => λ[Regex[In, M, ?] ~> λ[a => Gen[Stream[In]]]](outer.apply(_))
        )(fa)
    }

  def regexMMatchingStreamGen[In](
    matchGen: Match[In] => Gen[In]): RegexM[In, ?] ~> λ[a => Gen[Stream[In]]] =
    regexMatchingStreamGen(matchGen)

  def dietRegexMatchingStreamGen[In: Choose: Discrete: Order, Out](
    available: Diet[In]): RegexM[In, Out] => Gen[Stream[In]] =
    regexMMatchingStreamGen[In](dietMatchToGen(available, dietMatchingGen(_))).apply

  def genRegexMatch[In, M, Out](r: Regex[In, M, Out])(implicit
    rc: RegexCandidates[In, M]): Gen[Stream[In]] =
    rc.genMatchingStream(r)
}
