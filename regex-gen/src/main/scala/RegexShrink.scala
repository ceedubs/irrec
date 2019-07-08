package ceedubs.irrec
package regex

import cats.collections.{Diet, Discrete, Range}
import higherkindness.droste.{scheme, RAlgebra}
import higherkindness.droste.data.{Coattr, CoattrF}
import higherkindness.droste.data.prelude._
import org.scalacheck.Shrink
import cats.{Now, Order}
import cats.implicits._

object RegexShrink {

  def shrinkKleeneF[A]: KleeneF[(Regex[A], Stream[Regex[A]])] => Stream[Regex[A]] = {
    case KleeneF.Plus(l, r) =>
      l._1 #:: // only left
        r._1 #:: // only right
        (l._2.flatMap(x => r._2.map(x | _))) #::: // shrink both
        r._2.map(l._1 | _) #::: // shrink left
        l._2.map(_ | r._1) // shrink right
    case KleeneF.Times(l, r) =>
      l._1 #:: // only left
        r._1 #:: // only right
        (l._2.flatMap(x => r._2.map(x * _))) #::: // shrink both
        r._2.map(l._1 * _) #::: // shrink left
        l._2.map(_ * r._1) // shrink right
    case KleeneF.Star(g) => g._2 :+ g._1 :+ Regex.empty
    case KleeneF.Zero => Stream.empty
    case KleeneF.One => Stream.empty
  }

  def shrinkMatch[A: Discrete: Order]: Match[A] => Stream[Match[A]] = {
    case Match.Literal(_) => Stream.empty
    case Match.Wildcard() => Stream.empty
    case Match.MatchSet.Allow(allowed) =>
      shrinkDiet(allowed).filterNot(_.isEmpty).map(Match.MatchSet.allow(_))
    case Match.MatchSet.Forbid(forbidden) => shrinkDiet(forbidden).map(Match.MatchSet.forbid(_))
  }

  def shrinkRegexCoattrF[A: Discrete: Order: Shrink]
    : CoattrF[KleeneF, Match[A], (Regex[A], Stream[Regex[A]])] => Stream[Regex[A]] =
    CoattrF.un(_) match {
      case Left(ma) => shrinkMatch[A].apply(ma).map(Coattr.pure[KleeneF, Match[A]](_))
      case Right(kf) => shrinkKleeneF(kf)
    }

  def shrinkRegex[A: Discrete: Order: Shrink]: Regex[A] => Stream[Regex[A]] =
    scheme.zoo.para(RAlgebra(shrinkRegexCoattrF))

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

  def shrinkDiet[A: Discrete: Order](diet: Diet[A]): Stream[Diet[A]] = {
    implicit val rangeShrink: Shrink[Range[A]] = Shrink(shrinkRange(_))
    Shrink
      .shrink(dietRangeList(diet))
      .filter(_.nonEmpty)
      .map(ranges => ranges.foldMap(Diet.fromRange _))
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
}
