package ceedubs.irrec
package parse

// TODO
import ceedubs.irrec.regex.RegexC

import scala.reflect.macros.blackbox.Context
import fastparse._

object ParserMacros {
  def parseLiteralImpl(c: Context)(regex: c.Expr[String]): c.Expr[RegexC[String]] = {
    import c.universe._
    regex.tree match {
      case Literal(Constant(s: String)) =>
        Parser
          .parseRegex(s)
          .fold(
            err => c.abort(c.enclosingPosition, s"Error compiling regular expression: $err"),
            _ => reify(parse(regex.splice, Parser.regexExpr(_)).get.value))
      case _ =>
        c.abort(
          c.enclosingPosition,
          "Macro-based regular expression parsing only works on literal constant strings.")
    }
  }
}
