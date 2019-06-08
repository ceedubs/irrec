package ceedubs.irrec

import ceedubs.irrec.regex.Regex

package object parse {
  def regex(regex: String): Regex[Char] = macro ParserMacros.parseLiteralImpl
}
