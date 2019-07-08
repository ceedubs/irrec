package ceedubs.irrec
package regex

import Regex._

import cats.Eq
import cats.tests.CatsSuite
import org.scalacheck.Shrink, Shrink.shrink
import cats.collections.Diet

class RegexShrinkTests extends CatsSuite {
  // this is pretty hacky but I haven't yet written a meaningful equality for regexes.
  implicit private val eqIntRegex: Eq[Regex[Int]] = Eq.instance((x, y) => x.optimize == y.optimize)
  implicit private val eqCharRegex: Eq[Regex[Char]] = Eq.by(_.optimize.pprint)
  implicit val shrinkIntRegex: Shrink[Regex[Int]] = RegexShrink.shrinkForRegex
  implicit val shrinkCharRegex: Shrink[Regex[Char]] = RegexShrink.shrinkForRegex

  test("literals don't shrink") {
    val r: Regex[Int] = Regex.lit(2)
    val expected = List.empty[Regex[Int]]
    shrink(r).toList should ===(expected)
  }

  test("shrinking a wildcard") {
    val r: Regex[Int] = Regex.wildcard[Int]
    val expected = List.empty[Regex[Int]]
    shrink(r).toList should ===(expected)
  }

  test("shrinking plus") {
    val r: Regex[Char] = Regex.oneOf('1', '2') | Regex.oneOf('3', '4')
    val expected = List(
      // left
      Regex.oneOf('1', '2'),
      // right
      Regex.oneOf('3', '4'),
      // shrinking both
      Regex.lit('1') | Regex.lit('3'),
      Regex.lit('1') | Regex.lit('4'),
      Regex.lit('2') | Regex.lit('3'),
      Regex.lit('2') | Regex.lit('4'),
      // shrinking only right
      Regex.oneOf('1', '2') | Regex.lit('3'),
      Regex.oneOf('1', '2') | Regex.lit('4'),
      // shrinking only left
      Regex.lit('1') | Regex.oneOf('3', '4'),
      Regex.lit('2') | Regex.oneOf('3', '4')
    )
    shrink(r).toList should ===(expected)
  }

  test("shrinking times") {
    val r: Regex[Char] = Regex.oneOf('1', '2') * Regex.oneOf('3', '4')
    val expected = List(
      // left
      Regex.oneOf('1', '2'),
      // right
      Regex.oneOf('3', '4'),
      // shrinking both
      Regex.lit('1') * Regex.lit('3'),
      Regex.lit('1') * Regex.lit('4'),
      Regex.lit('2') * Regex.lit('3'),
      Regex.lit('2') * Regex.lit('4'),
      // shrinking only right
      Regex.oneOf('1', '2') * Regex.lit('3'),
      Regex.oneOf('1', '2') * Regex.lit('4'),
      // shrinking only left
      Regex.lit('1') * Regex.oneOf('3', '4'),
      Regex.lit('2') * Regex.oneOf('3', '4')
    )
    shrink(r).toList should ===(expected)
  }

  test("shrinking a * should shrink the inner regex and allow empty") {
    val r: Regex[Char] = Regex.oneOf('1', '2').star
    val expected =
      List(Regex.lit('1'), Regex.lit('2'), Regex.oneOf('1', '2'), Regex.empty[Match[Char]])
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

  test("shrink positive MatchSet") {
    val r: Regex[Char] = inSet(Diet.one('a') + 'b' + 'c')
    val expected = List(
      Diet.one('a') + 'b',
      Diet.one('a'),
      Diet.one('b') + 'c',
      Diet.one('c')
    ).map(inSet(_))
    shrink(r).toList should ===(expected)
  }

  test("shrink negative MatchSet") {
    val r: Regex[Char] = notInSet(Diet.one('a') + 'b' + 'c')
    val expected = List(
      Diet.empty[Char],
      Diet.one('a') + 'b',
      Diet.one('a'),
      Diet.one('b') + 'c',
      Diet.one('c')
    ).map(notInSet(_))
    shrink(r).toList should ===(expected)
  }
}
