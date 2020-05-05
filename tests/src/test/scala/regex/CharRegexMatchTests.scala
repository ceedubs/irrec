package ceedubs.irrec
package regex

import ceedubs.irrec.parse.{regex => parse}
import ceedubs.irrec.regex.char._
import ceedubs.irrec.regex.combinator._

import cats.implicits._
import org.scalacheck.Gen

class CharRegexMatchTests extends IrrecSuite {
  test("character class literal match middle") {
    parse("a[bd-fhj]l").compile.parseOnlyS("ahl") should ===(Some("ahl"))
  }

  test("character class literal match end") {
    parse("a[bd-fh]j").compile.parseOnlyS("ahj") should ===(Some("ahj"))
  }

  test("character class literal non-match") {
    parse("a[bd-fh]j").compile.parseOnlyS("axj") should ===(None)
  }

  test("character class range match beginning") {
    parse("a[d-fh]j").compile.parseOnlyS("aej") should ===(Some("aej"))
  }

  test("character class range match end") {
    parse("a[bd-f]j").compile.parseOnlyS("aej") should ===(Some("aej"))
  }

  test("character class range non-match") {
    parse("a[d-fh]j").compile.parseOnlyS("axj") should ===(None)
  }

  test("character class range match low") {
    parse("a[bd-fh]j").compile.parseOnlyS("adj") should ===(Some("adj"))
  }

  test("character class range match high") {
    parse("a[bd-fh]j").compile.parseOnlyS("afj") should ===(Some("afj"))
  }

  test("digit character single match")(digit.compile.parseOnlyS("2") should ===(Some(2)))

  test("digit character non-match")(digit.compile.parseOnlyS("a") should ===(None))

  test("non-digit character single match") {
    nonDigit.compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("non-digit character non-match")(nonDigit.compile.parseOnlyS("3") should ===(None))

  test("word character single match")(wordChar.compile.parseOnlyS("a") should ===(Some('a')))

  test("word character non-match")(wordChar.compile.parseOnlyS("%") should ===(None))

  test("non-word character single match") {
    nonWordChar.compile.parseOnlyS("%") should ===(Some('%'))
  }

  test("non-word character non-match")(nonWordChar.compile.parseOnlyS("a") should ===(None))

  test("whitespace character single match") {
    whitespaceChar.compile.parseOnlyS(" ") should ===(Some(' '))
  }

  test("whitespace character non-match")(whitespaceChar.compile.parseOnlyS("%") should ===(None))

  test("whitespace character negated range match") {
    lit('a').product(nonWhitespaceChar).product(lit('c')).compile.parseOnlyS("abc") should ===(
      Some((('a', 'b'), 'c')))
  }

  test("whitespace character negated range non-match") {
    lit('a').product(nonWhitespaceChar).product(lit('c')).compile.parseOnlyS("a c") should ===(None)
  }

  test("non-whitespace character single match") {
    nonWhitespaceChar.compile.parseOnlyS("a") should ===(Some('a'))
  }

  test("non-whitespace character non-match") {
    nonWhitespaceChar.compile.parseOnlyS(" ") should ===(None)
  }

  test("word character match") {
    val gen = Gen.oneOf(Gen.alphaNumChar, Gen.const('_'))
    val rc = wordChar.compile
    forAll(gen)(c => rc.parseOnlyS(c.toString) should ===(Some(c)))
  }

  test("whitespace character match") {
    val gen = Gen.oneOf('\t', '\n', '\f', '\r', ' ')
    val rc = whitespaceChar.compile
    forAll(gen)(c => rc.parseOnlyS(c.toString) should ===(Some(c)))
  }

  test("withMatchedS match") {
    val r = lit('a') <* lit('b')
    r.withMatchedS.compile.parseOnlyS("ab") should ===(Some(("ab", 'a')))
  }

  test("withMatchedS non-match") {
    val r = lit('a') <* lit('b')
    r.withMatchedS.compile.parseOnlyS("b") should ===(None)
  }

  test("matchedS match") {
    val r = lit('a') <* lit('b')
    r.matchedS.compile.parseOnlyS("ab") should ===(Some("ab"))
  }

  test("matchedS non-match") {
    val r = lit('a') <* lit('b')
    r.matchedS.compile.parseOnlyS("b") should ===(None)
  }
}
