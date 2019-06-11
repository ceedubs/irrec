package ceedubs.irrec
package parse

import ceedubs.irrec.regex.Regex

import scala.reflect.macros.blackbox.Context
import fastparse._
import fastparse.Parsed.Failure
import fastparse.Parsed.Success

object ParserMacros {
  def parseLiteralImpl(c: Context)(regex: c.Expr[String]): c.Expr[Regex[Char]] = {
    import c.universe._
    regex.tree match {
      case Literal(Constant(s: String)) => parse(s, Parser.regexExpr(_), verboseFailures = true) match {
        case f @ Failure(_, _, _) =>
          c.abort(c.enclosingPosition, s"Error compiling regular expression: ${f.msg}")
        case Success(_, _) =>
          reify(parse(regex.splice, Parser.regexExpr(_)).get.value)
      }
      case _ => c.abort(c.enclosingPosition, "Macro-based regular expression parsing only works on literal constant strings.")
    }
  }
}
