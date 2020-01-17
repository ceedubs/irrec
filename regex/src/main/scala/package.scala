package ceedubs.irrec

package object regex {
  type RegexM[In, Out] = Regex[In, Match[In], Out]
  type RegexC[Out] = Regex[Char, Match[Char], Out]
}
