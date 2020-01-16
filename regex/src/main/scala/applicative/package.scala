package ceedubs.irrec.regex

// TODO
import applicative.Regex.RegexC

// TODO move
package object applicative {
  implicit def toRegexCOps[Out](r: RegexC[Out]): RegexCOps[Out] = new RegexCOps(r)
}
