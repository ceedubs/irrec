package ceedubs.irrec

import ceedubs.irrec.regex.Regex
import ceedubs.irrec.regex.applicative.{Regex => Regex2}

package object parse {
  def regex(regex: String): Regex[Char] = macro ParserMacros.parseLiteralImpl

  def regex2(regex: String): Regex2.Regex[Char, String] = macro ParserMacros.parseLiteral2Impl
}
