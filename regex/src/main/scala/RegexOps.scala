package ceedubs.irrec
package regex

import cats.Foldable
import java.util.regex.Pattern

final class KleeneOps[A](private val r: Kleene[A]) extends AnyVal {

  def |(o: Kleene[A]): Kleene[A] = Regex.or(r, o)

  def *(o: Kleene[A]): Kleene[A] = Regex.andThen(r, o)

  def oneOrMore: Kleene[A] = Regex.oneOrMore(r)

  def star: Kleene[A] = Regex.star(r)

  def count(n: Int): Kleene[A] = Regex.count(n, r)

  def repeat(minInclusive: Int, maxInclusive: Int): Kleene[A] = Regex.repeat(minInclusive, maxInclusive, r)

  def optimize: Kleene[A] = KleeneOptimization.optimizeKleene(r)
}

final class RegexOps[A](private val r: Regex[A]) extends AnyVal {
  def matcher[F[_]](implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean =
    Regex.matcher(r)
}

final class CharRegexOps(private val r: Regex[Char]) extends AnyVal {
  def stringMatcher: String => Boolean = Regex.stringMatcher(r)

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)

  def pprint: String = RegexPrettyPrinter.pprintCharRegex(r)
}
