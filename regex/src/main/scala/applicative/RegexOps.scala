package ceedubs.irrec
package regex
package applicative

// TODO
import Regex.RegexC

import cats.implicits._
import java.util.regex.Pattern

// TODO package

final class RegexCOps[Out](private val r: RegexC[Out]) extends AnyVal {
  def pprint: String = RegexPrettyPrinter.pprintRE(r)

  def withMatchedS: RegexC[(String, Out)] = r.withMatched.map {
    case (s, o) => (s.mkString_(""), o)
  }

  def matchedS: RegexC[String] = r.matched.map(_.mkString_(""))

  def toPattern: Pattern = Pattern.compile(pprint, Pattern.DOTALL)
}
