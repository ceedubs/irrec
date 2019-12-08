package ceedubs.irrec
package bench

import regex._, Regex._

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Thread)
class RepeatBenchmarks {
  val length = 500
  val irrecR = manyRepeats
  val irrecMatcher: String => Boolean = irrecR.stringMatcher
  val mediumLengthMatch: String = "b" * 50
  val longMatch: String = "b" * length
  val longNonMatch: String = "b" * (length + 1)

  @Benchmark
  def manyRepeats: Regex[Char] =
    (lit('a') | lit('b')).repeat(10, Some(length))

  @Benchmark
  def irrecMatchMediumRepeats: Boolean =
    irrecMatcher(mediumLengthMatch)

  @Benchmark
  def irrecMatchLong: Boolean =
    irrecMatcher(longMatch)

  @Benchmark
  def irrecLongNonMatch: Boolean =
    irrecMatcher(longNonMatch)
}
