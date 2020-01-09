package ceedubs.irrec
package parse

import ceedubs.irrec.regex.Regex
import ceedubs.irrec.regex.applicative.{Regex => Regex2}

import scala.reflect.macros.blackbox.Context
import fastparse._

object ParserMacros {
  def parseLiteralImpl(c: Context)(regex: c.Expr[String]): c.Expr[Regex[Char]] = {
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

  def parseLiteral2Impl(c: Context)(regex: c.Expr[String]): c.Expr[Regex2.Regex[Char, Unit]] = {
    import c.universe._
    regex.tree match {
      case Literal(Constant(s: String)) =>
        Parser2
          .parseRegex(s)
          .fold(
            err => c.abort(c.enclosingPosition, s"Error compiling regular expression: $err"),
            _ => reify(parse(regex.splice, Parser2.regexExpr(_)).get.value))
      case _ =>
        c.abort(
          c.enclosingPosition,
          "Macro-based regular expression parsing only works on literal constant strings.")
    }
  }
}
