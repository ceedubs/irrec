package ceedubs.irrec
package regex

import cats.Foldable
import cats.implicits._
import java.util.regex.Pattern

// TODO package

final class RegexOps[In, M, Out](private val r: Regex[In, M, Out]) extends AnyVal {
  def matcher[F[_]: Foldable]: F[In] => Boolean = Regex.matcher(r)
}

final class RegexCOps[Out](private val r: RegexC[Out]) extends AnyVal {
  def pprint: String = RegexPrettyPrinter.pprint(r)

  def withMatchedS: RegexC[(String, Out)] = r.withMatched.map {
    case (s, o) => (s.mkString_(""), o)
  }

  def matchedS: RegexC[String] = r.matched.map(_.mkString_(""))

  def stringMatcher: String => Boolean = {
    val m = Regex.matcher(r)(IndexedSeqFoldable.instance)
    m(_)
  }

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)
}
