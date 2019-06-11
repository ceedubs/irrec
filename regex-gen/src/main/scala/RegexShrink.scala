package ceedubs.irrec
package regex

import qq.droste.{AlgebraM, scheme}
import qq.droste.data.{Coattr, CoattrF}
import qq.droste.data.prelude._
import org.scalacheck.Shrink
import cats.implicits._

object RegexShrink {

  def shrinkKleeneF[A]: KleeneF[Regex[A]] => Stream[Regex[A]] = {
    case KleeneF.Plus(l, r) => l #:: r #:: (l | r) #:: Stream.empty
    case KleeneF.Times(l, r) => l #:: r #:: (l * r) #:: Stream.empty
    case KleeneF.Star(g) => Stream(g)
    case KleeneF.Zero => Stream.empty
    case KleeneF.One => Stream.empty
  }

  def shrinkMatch[A](implicit shrinkA: Shrink[A]): Match[A] => Stream[Match[A]] = {
    case Match.Literal(expected) => shrinkA.shrink(expected).map(Match.Literal(_))
    case Match.Wildcard => Stream.empty
    case Match.Range(l, r) =>
      val shrunkLeft = shrinkA.shrink(l).map(l2 => Match.Range(l2, r))
      val shrunkRight = shrinkA.shrink(r).map(r2 => Match.Range(l, r2))
      shrunkLeft append shrunkRight
    case Match.NoneOf(_) => Stream.empty // TODO ceedubs implement
  }

  def shrinkRegexCoattrF[A:Shrink]: CoattrF[KleeneF, Match[A], Regex[A]] => Stream[Regex[A]] =
    CoattrF.un(_) match {
      case Left(ma) => shrinkMatch[A].apply(ma).map(Coattr.pure[KleeneF, Match[A]](_))
      case Right(kf) => shrinkKleeneF(kf)
    }

  def shrinkRegexCoattrFAlgebra[A:Shrink]: AlgebraM[Stream, CoattrF[KleeneF, Match[A], ?], Regex[A]]  = AlgebraM(shrinkRegexCoattrF[A])

  def shrinkRegex[A: Shrink]: Regex[A] => Stream[Regex[A]] =
    scheme.cataM(shrinkRegexCoattrFAlgebra[A])

  /**
   * This will "shrink" a regular expression down into similar but simpler regular expressions.  It
   * isn't `implicit`, because in Scalacheck shrinking isn't integrated into generators which can
   * lead to confusing results. If you'd like to shrink your regular expression to debug it, you can
   * create a local `Shrink` instance with something like:
   *
   * {{{
   * implicit val shrinkForCharRegex: Shrink[Regex[Char]] = RegexShrink.shrinkForRegex`
   * }}}
   */
  def shrinkForRegex[A: Shrink]: Shrink[Regex[A]] = Shrink(shrinkRegex[A])
}
