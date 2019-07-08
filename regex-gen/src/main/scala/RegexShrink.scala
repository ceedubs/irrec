package ceedubs.irrec
package regex

import cats.collections.{Diet, Discrete, Range}
import qq.droste.{scheme, AlgebraM}
import qq.droste.data.{Coattr, CoattrF}
import qq.droste.data.prelude._
import org.scalacheck.Shrink
import cats.{Now, Order}
import cats.implicits._

object RegexShrink {

  def shrinkKleeneF[A]: KleeneF[Regex[A]] => Stream[Regex[A]] = {
    case KleeneF.Plus(l, r) => l #:: r #:: (l | r) #:: Stream.empty
    case KleeneF.Times(l, r) => l #:: r #:: (l * r) #:: Stream.empty
    case KleeneF.Star(g) => Stream(g)
    case KleeneF.Zero => Stream.empty
    case KleeneF.One => Stream.empty
  }

  // TODO ceedubs do we actually want to use the Shrink[A]?
  def shrinkMatch[A: Discrete: Order](implicit shrinkA: Shrink[A]): Match[A] => Stream[Match[A]] = {
    case Match.Literal(expected) => shrinkA.shrink(expected).map(Match.Literal(_))
    case Match.Wildcard() => Stream.empty
    case Match.MatchSet.Allow(allowed) => shrinkDiet(allowed).map(Match.MatchSet.allow(_))
    case Match.MatchSet.Forbid(forbidden) => shrinkDiet(forbidden).map(Match.MatchSet.forbid(_))
  }

  def shrinkDiet[A: Discrete: Order](diet: Diet[A]): Stream[Diet[A]] = {
    implicit val rangeShrink: Shrink[Range[A]] = Shrink(shrinkRange(_))
    Shrink.shrink(dietRangeList(diet)).map(ranges => ranges.foldMap(Diet.fromRange _))
  }

  // TODO ceedubs add something to cats collections so we don't need this.
  private def dietRangeList[A](diet: Diet[A]): List[Range[A]] =
    diet.foldRightRange(Now(List.empty[Range[A]]))((r, rs) => rs.map(r :: _)).value

  def shrinkRange[A](
    range: Range[A])(implicit discreteA: Discrete[A], orderA: Order[A]): Stream[Range[A]] =
    Stream
      .iterate(discreteA.pred(range.end))(e => discreteA.pred(e)) // decrease end
      .takeWhile(_ >= range.start)
      .map(e => Range(range.start, e)) append
      Stream
        .iterate(discreteA.succ(range.start))(s => discreteA.succ(s)) // increase start
        .takeWhile(_ <= range.end)
        .map(s => Range(s, range.end))

  def shrinkRegexCoattrF[A: Discrete: Order: Shrink]
    : CoattrF[KleeneF, Match[A], Regex[A]] => Stream[Regex[A]] =
    CoattrF.un(_) match {
      case Left(ma) => shrinkMatch[A].apply(ma).map(Coattr.pure[KleeneF, Match[A]](_))
      case Right(kf) => shrinkKleeneF(kf)
    }

  def shrinkRegexCoattrFAlgebra[A: Discrete: Order: Shrink]
    : AlgebraM[Stream, CoattrF[KleeneF, Match[A], ?], Regex[A]] = AlgebraM(shrinkRegexCoattrF[A])

  // TODO ceedubs we have an issue where if the left side returns an empty Stream upon shrinking,
  // then we flatMap and never include the right.
  def shrinkRegex[A: Discrete: Order: Shrink]: Regex[A] => Stream[Regex[A]] =
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
  def shrinkForRegex[A: Discrete: Order: Shrink]: Shrink[Regex[A]] = Shrink(shrinkRegex[A])
}
