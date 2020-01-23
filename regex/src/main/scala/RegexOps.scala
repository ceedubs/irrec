package ceedubs.irrec
package regex

import cats.Foldable
import cats.data.Chain
import java.util.regex.Pattern

final class RegexOps[In, M, Out](private val r: Regex[In, M, Out]) extends AnyVal {
  def |(o: Regex[In, M, Out]): Regex[In, M, Out] = combinator.or(r, o)

  def either[Out2](o: Regex[In, M, Out2]): Regex[In, M, Either[Out, Out2]] =
    combinator.either(r, o)

  def star[Out2](g: Greediness, z: Out2)(fold: (Out2, Out) => Out2) = combinator.star(r, g, z)(fold)

  def matcher[F[_]: Foldable]: F[In] => Boolean = Regex.matcher(r)

  def compile: ParseState[In, Out] = Regex.compile(r)

  def optional: Regex[In, M, Option[Out]] = combinator.optional(r)

  def withMatched: Regex[In, M, (Chain[In], Out)] =
    combinator.withMatched(r)

  def matched: Regex[In, M, Chain[In]] = combinator.matched(r)
}

final class RegexCOps[Out](private val r: RegexC[Out]) extends AnyVal {
  def pprint: String = RegexPrettyPrinter.pprint(r)

  def withMatchedS: RegexC[(String, Out)] = char.withMatchedS(r)

  def matchedS: RegexC[String] = char.matchedS(r)

  def stringMatcher: String => Boolean = {
    val m = Regex.matcher(r)(IndexedSeqFoldable.instance)
    m(_)
  }

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)
}
