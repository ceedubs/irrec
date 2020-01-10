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
  def regexMatchingStreamGen[In](matchGen: Match[In] => Gen[In]): Regex[In, ?] ~> λ[a => Gen[Stream[In]]] = new (Regex[In, ?] ~> λ[a => Gen[Stream[In]]]) {
    def apply[A](fa: RE[In,Match[In],A]): Gen[Stream[In]] = fa match {
      case RE.Or(alternatives) => Gen.oneOf(alternatives.toList).flatMap(apply)
      case RE.Fail() => Gen.fail
      case RE.Match(m, _) => matchGen(m).map(Stream(_))
      case RE.Star(r, _, _, _) => Gen.containerOf[Stream, Stream[In]](apply(r)).map(_.flatten)
      case RE.Eps => Gen.const(Stream.empty)
      case RE.AndThen(l, r) => apply(l).map2(apply(r))(_ ++ _)
      case RE.FMap(r, _) => apply(r)
      // This is gross but _should_ be safe.
      // I seem to be running into https://github.com/scala/bug/issues/10292
      case v => apply(v.asInstanceOf[RE.Void[In, Match[In], _]].r)
    }
  }
}
