package ceedubs.irrec
package regex

import cats.Eq
import cats.tests.CatsSuite
import org.scalacheck.Shrink, Shrink.shrink

class RegexShrinkTests extends CatsSuite {
  // this is pretty hacky but I haven't yet written a meaningful equality for regexes.
  implicit private val eqIntRegex: Eq[Regex[Int]] = Eq.fromUniversalEquals
  implicit val shrinkIntRegex: Shrink[Regex[Int]] = RegexShrink.shrinkForRegex
  implicit val shrinkCharRegex: Shrink[Regex[Char]] = RegexShrink.shrinkForRegex

  test("shrinking a literal") {
    val r: Regex[Int] = Regex.lit(2)
    val expected = List(Regex.lit(1), Regex.lit(-1), Regex.lit(0))
    shrink(r).toList should ===(expected)
  }

  test("shrinking a wildcard") {
    val r: Regex[Int] = Regex.wildcard[Int]
    val expected = List.empty[Regex[Int]]
    shrink(r).toList should ===(expected)
  }

  test("shrinking a plus") {
    val r: Regex[Int] = Regex.lit(1) | Regex.lit(1)
    val expected = List(
      // shrinking left
      Regex.lit(0),
      // shrinking right
      Regex.lit(0),
      // shrinking both
      Regex.lit(0) | Regex.lit(0))
    shrink(r).toList should ===(expected)
  }

  test("shrinking a times") {
    val r: Regex[Int] = Regex.lit(1) * Regex.lit(1)
    val expected = List(
      // shrinking left
      Regex.lit(0),
      // shrinking right
      Regex.lit(0),
      // shrinking both
      Regex.lit(0) * Regex.lit(0))
    shrink(r).toList should ===(expected)
  }

  test("shrinking a * should shrink the inner regex") {
    val r: Regex[Int] = Regex.lit(2).star
    val expected = List(Regex.lit(1), Regex.lit(-1), Regex.lit(0))
    shrink(r).toList should ===(expected)
  }

  test("zero cannot be shrunk") {
    val r: Regex[Int] = Regex.impossible
    val expected = List.empty[Regex[Int]]
    shrink(r).toList should ===(expected)
  }

  test("one cannot be shrunk") {
    val r: Regex[Int] = Regex.empty
    val expected = List.empty[Regex[Int]]
    shrink(r).toList should ===(expected)
  }
}
