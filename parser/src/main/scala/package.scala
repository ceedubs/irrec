package ceedubs.irrec

import ceedubs.irrec.regex.RegexC

package object parse {
  def regex(regex: String): RegexC[String] = macro ParserMacros.parseLiteralImpl
}
