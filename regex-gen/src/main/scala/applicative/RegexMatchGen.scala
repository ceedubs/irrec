package ceedubs.irrec
package regex.applicative
// TODO package

import Regex.Regex
import ceedubs.irrec.regex.Match
import ceedubs.irrec.regex.ScalacheckSupport._

import cats.~>
import org.scalacheck.Gen
import cats.implicits._

object RegexMatchGen {
  def regexMatchingStreamGen[In](
    matchGen: Match[In] => Gen[In]): Regex[In, ?] ~> λ[a => Gen[Stream[In]]] =
    new (Regex[In, ?] ~> λ[a => Gen[Stream[In]]]) { outer =>
      // TODO think about whether kind-projector literals are actually making this any cleaner
      def apply[Out](fa: RE[In, Match[In], Out]): Gen[Stream[In]] =
        RE.fold[In, Match[In], Out, Gen[Stream[In]]](
          eps = _ => Gen.const(Stream.empty),
          fail = () => Gen.fail,
          mappedMatch = (m, _) => matchGen(m).map(Stream(_)),
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
}
