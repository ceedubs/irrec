package ceedubs.irrec
package bench

import regex._, combinator._
import Greediness.Greedy

import cats.implicits._
import java.util.regex.Pattern
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class ZeroStarStarABenchmarks {
  val manyZeros: String = "0" * 20
  val java: Pattern = Pattern.compile("(0*)*A")
  val irrec: RegexC[Unit] = lit('0').chain(Greedy).chain(Greedy).void <* lit('A')
  val irrecMatcher: String => Boolean = irrec.stringMatcher

  @Benchmark
  def javaMatchManyZeros: Boolean =
    java.matcher(manyZeros).matches

  @Benchmark
  def irrecMatchManyZeros: Boolean =
    irrecMatcher(manyZeros)
}
