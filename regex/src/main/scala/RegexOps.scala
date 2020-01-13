package ceedubs.irrec
package regex

import cats.{Foldable, Order}
import cats.data.Chain
import java.util.regex.Pattern
import cats.collections.Discrete

final class KleeneOps[A](private val r: Kleene[A]) extends AnyVal {
  def |(o: Kleene[A]): Kleene[A] = Regex.or(r, o)

  def *(o: Kleene[A]): Kleene[A] = Regex.andThen(r, o)

  def oneOrMore: Kleene[A] = Regex.oneOrMore(r)

  def star: Kleene[A] = Regex.star(r)

  def count(n: Int): Kleene[A] = Regex.count(n, r)

  def optional: Kleene[A] = Regex.optional(r)

  def repeat(minInclusive: Int, maxInclusive: Option[Int]): Kleene[A] =
    Regex.repeat(minInclusive, maxInclusive, r)

  def !(implicit ev: CaptureAs[A]): CapturingKleeneA[A, ev.In, Chain[ev.In]] =
    CapturingKleeneA.lift(r)

  def captureAs[L](label: L): CapturingKleene[L, A] =
    CapturingKleene.labeledKleene(LabeledKleene(label, r))
}

final class RegexOps[A](private val r: Regex[A]) extends AnyVal {
  // TODO
  import applicative.RE
  def matcher[F[_]](implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean = {
    implicit val orderA = cats.Order.fromOrdering(orderingA)
    val re = RE.compile(RE.ofRegex(r))
    s => re.parseOnly(s).nonEmpty
  }

  def optimize(implicit discreteA: Discrete[A], orderA: Order[A]): Regex[A] =
    RegexOptimization.optimizeRegex[A].apply(r)
}

final class CharRegexOps(private val r: Regex[Char]) extends AnyVal {
  // TODO
  import applicative.RE
  import cats.implicits._
  def stringMatcher: String => Boolean = {
    val re = RE.compile(RE.ofRegex(r))
    s => re.parseOnly(s.toList).nonEmpty
  }

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)

  def pprint: String = RegexPrettyPrinter.pprintCharRegex(r)
}
