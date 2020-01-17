package ceedubs.irrec
package regex

// TODO
import Regex.{RegexC, RegexG}

import cats.Foldable
import cats.implicits._
import java.util.regex.Pattern

// TODO package

final class RegexGOps[In, M, Out](private val r: RegexG[In, M, Out]) extends AnyVal {
  def matcher[F[_]: Foldable]: F[In] => Boolean = RE.matcher(r)
}

final class RegexCOps[Out](private val r: RegexC[Out]) extends AnyVal {
  def pprint: String = RegexPrettyPrinter.pprintRE(r)

  def withMatchedS: RegexC[(String, Out)] = r.withMatched.map {
    case (s, o) => (s.mkString_(""), o)
  }

  def matchedS: RegexC[String] = r.matched.map(_.mkString_(""))

  def stringMatcher: String => Boolean = {
    val m = RE.matcher(r)(IndexedSeqFoldable.instance)
    m(_)
  }

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)
}
