package ceedubs.irrec
package bench

import regex._, Regex._
import Greediness.Greedy

import cats.data.Chain
import cats.implicits._
import java.util.regex.Pattern
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class RepeatCountRegexBenchmarks {
  val matchingString: String = ("a" * 10) + ("b" * 13)
  val nonMatchingString: String = ("a" * 9) + ("b" * 9)
  val longNonMatchingString: String = matchingString + ("c" * 20)
  val java: Pattern = Pattern.compile("(a|b){20,25}")
  val irrec: Regex[Char, Chain[Char]] = oneOf('a', 'b').repeat(20, Some(25), Greedy)
  val irrecMatcher: String => Boolean = irrec.stringMatcher

  @Benchmark
  def javaMatch: Boolean =
    java.matcher(matchingString).matches

  @Benchmark
  def irrecMatch: Boolean =
    irrecMatcher(matchingString)

  @Benchmark
  def javaNonMatch: Boolean =
    java.matcher(nonMatchingString).matches

  @Benchmark
  def irrecNonMatch: Boolean =
    irrecMatcher(nonMatchingString)

  @Benchmark
  def javaLongNonMatch: Boolean =
    java.matcher(longNonMatchingString).matches

  @Benchmark
  def irrecLongNonMatch: Boolean =
    irrecMatcher(longNonMatchingString)
}
