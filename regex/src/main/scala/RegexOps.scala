package ceedubs.irrec
package regex

import cats.Foldable

final case class RegexOps[A](private val r: Regex[A]) extends AnyVal {
  def matcher[F[_]](implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean =
    Regex.matcher(r)
}

final case class CharRegexOps(private val r: Regex[Char]) extends AnyVal {
  def stringMatcher: String => Boolean = Regex.stringMatcher(r)
}
