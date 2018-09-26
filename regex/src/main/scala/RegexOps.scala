package ceedubs.irrec
package regex

import cats.Foldable

final case class RegexOps[A](private val r: Regex[A]) extends AnyVal {
  def matcher[F[_]](implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean =
    Regex.matcher(r)

  def |(o: Regex[A]): Regex[A] = Regex.or(r, o)

  def *(o: Regex[A]): Regex[A] = Regex.andThen(r, o)

  def oneOrMore: Regex[A] = Regex.oneOrMore(r)

  def star: Regex[A] = Regex.star(r)

  def count(n: Int): Regex[A] = Regex.count(n, r)
}

final case class CharRegexOps(private val r: Regex[Char]) extends AnyVal {
  def stringMatcher: String => Boolean = Regex.stringMatcher(r)
}
