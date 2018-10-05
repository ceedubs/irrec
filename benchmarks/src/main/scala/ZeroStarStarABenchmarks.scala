package ceedubs.irrec
package bench

import regex._, Regex._
import java.util.regex.Pattern
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class ZeroStarStarABenchmarks {
  val manyZeros: String = "0" * 20
  val java: Pattern = Pattern.compile("(0*)*A")
  val irrec: Regex[Char] = lit('0').star.star * lit('A')
  val irrecMatcher: String => Boolean = irrec.stringMatcher
  val irrecOptimizedMatcher: String => Boolean =
    irrec.optimize.stringMatcher

  @Benchmark
  def javaMatchManyZeros: Boolean =
    java.matcher(manyZeros).matches

  @Benchmark
  def irrecMatchManyZeros: Boolean =
    irrecMatcher(manyZeros)

  @Benchmark
  def irrecOptimizedMatchManyZeros: Boolean =
    irrecOptimizedMatcher(manyZeros)
}
