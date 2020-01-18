package ceedubs.irrec
package bench

import cats.implicits._
import regex._, combinator._
import java.util.regex.Pattern
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class AndThenAndOrRegexBenchmarks {
  val matchingString: String = "abcefg"
  val nonMatchingString: String = "abcdefg"
  val longNonMatchingString: String = "abcdefg" * 10
  val java: Pattern = Pattern.compile("ab(c|d)efg")
  val irrec: RegexC[Unit] = seq("ab").void <* oneOf('c', 'd') <* seq("efg")
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
