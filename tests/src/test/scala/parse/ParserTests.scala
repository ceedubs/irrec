package ceedubs.irrec
package parse

import ceedubs.irrec.regex._
import CharRegexGen._
import ceedubs.irrec.parse.{regex => parse}
import Match.MatchSet
// TODO ceeeubs
//import Parser.{parseCapturingRegex, parseRegex}
import Parser.parseRegex

import fastparse._
import ceedubs.irrec.regex.Regex._
import org.scalatest.compatible.Assertion
import cats.collections.{Diet, Range}

// TODO ceedubs add tests for capturing
class ParserTests extends IrrecSuite {
  test("regex parsing works for single literal") {
    val expected = Regex.lit('a')
    val r = parse("a")
    sameRegex(r, expected)
  }

  test("regex parsing works for literal then literal") {
    val expected = Regex.lit('a') * Regex.lit('b')
    val r = parse("ab")
    sameRegex(r, expected)
  }
  test("regex parsing works for literal or literal") {
    val expected = Regex.lit('a') | Regex.lit('b')
    val r = parse("a|b")
    sameRegex(r, expected)
  }

  test("regex parsing matches disjunction of literal sequences") {
    val expected = Regex.seq("ab") | Regex.seq("bc")
    val r = parse("ab|bc")
    sameRegex(r, expected)
  }

  test("regex parsing matches unnecessary outer parens") {
    val expected = Regex.seq("ab")
    val r = parse("(?:ab)")
    sameRegex(r, expected)
  }

  // TODO ceedubs
  //test("capturing regex parsing matches unnecessary outer parens") {
  //  val expected = Regex.seq("ab").capture
  //  val r = parseCapturingRegex("(ab)")
  //  sameRegex(r, expected)
  //}

  test("regex parsing handles non-capturing parens") {
    val expected = lit('a') * seq("bc") * lit('d')
    val r = parse("a(?:bc)d")
    sameRegex(r, expected)
  }

  test("regex parsing matches literal*") {
    val expected = Regex.lit('a').star
    val r = parse("a*")
    sameRegex(r, expected)
  }

  test("regex parsing matches literal* then another matcher") {
    val expected = Regex.lit('a').star * lit('b')
    val r = parse("a*b")
    sameRegex(r, expected)
  }

  test("regex parsing handles precedence with *") {
    val expected = Regex.lit('a') | (Regex.lit('c') * Regex.lit('d').star)
    val r = parse("a|cd*")
    sameRegex(r, expected)
  }

  test("regex parsing respects parens") {
    val expected = (Regex.lit('a') | Regex.lit('c')) * Regex.lit('d')
    val r = parse("(?:a|c)d")
    sameRegex(r, expected)
  }

  test("regex parsing is fine with nested parens") {
    val expected = (Regex.lit('a') | Regex.lit('c')) * Regex.lit('d')
    val r = parse("(?:(?:(?:a|(?:c)))d)")
    sameRegex(r, expected)
  }

  test("regex parsing respects spaces") {
    val expected = seq("ab cd")
    val r = parse("ab cd")
    sameRegex(r, expected)
  }

  test("regex parsing respects tabs") {
    val expected = seq("ab\tcd")
    val r = parse("ab\tcd")
    sameRegex(r, expected)
  }

  test("regex parsing supports character classes") {
    val expected = lit('a') * Regex.range('b', 'd') * lit('e')
    val r = parse("a[bcd]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports escaped special characters within character classes") {
    val expected = lit('a') * inSet(Diet.one('*')) * lit('e')
    val r = parse("""a[\*]e""")
    sameRegex(r, expected)
  }

  test("regex parsing supports single negated character classes") {
    val expected = lit('a') * Regex.noneOf('b') * lit('c')
    val r = parse("""a[^b]c""")
    sameRegex(r, expected)
  }

  test("regex parsing supports escaped special characters within negative character classes") {
    val expected = lit('a') * Regex.noneOf('*') * lit('e')
    val r = parse("""a[^\*]e""")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges") {
    val expected = lit('a') * range('b', 'd') * lit('e')
    val r = parse("a[b-d]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges with multiple ranges and non-ranges") {
    val charClass = inSet(Diet.fromRange(Range('b', 'e')) + 'g' + Range('i', 'k'))
    val expected = lit('a') * charClass * lit('e')
    val r = parse("a[b-degi-k]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges with negative character classes") {
    val negated = notInSet(Diet.fromRange(Range('b', 'd')) + 'e' + 'g' + Range('i', 'k'))
    val expected = lit('a') * negated * lit('e')

    val r = parse("a[^b-degi-k]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports exact repeat counts") {
    val expected = lit('a') * lit('b').repeat(3, Some(3)) * lit('e')
    val r = parse("ab{3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges starting with 1") {
    val expected = lit('a') * lit('b').repeat(1, Some(3)) * lit('e')
    val r = parse("ab{1,3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges starting with 0") {
    val expected = lit('a') * lit('b').repeat(0, Some(3)) * lit('e')
    val r = parse("ab{0,3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges with unbounded upper limit") {
    val expected = lit('a') * lit('b').repeat(1, None) * lit('e')
    val r = parse("ab{1,}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports optional elements") {
    val expected = lit('a') * lit('b').optional * lit('e')
    val r = parse("ab?e")
    sameRegex(r, expected)
  }

  test("regex parsing handles complex nested expressions") {
    val expected = (lit('a') | (lit('b') * wildcard.star)) * lit('d')
    val r = parse("(?:a|b.*)d")
    sameRegex(r, expected)
  }

  test("regex parsing handles digit classes") {
    val expected = lit('a') * inSet(Diet.fromRange(Range('0', '9')) + Range('b', 'c'))
    sameRegex(parse("""a[b\dc]"""), expected)
    sameRegex(parse("""a[b[:digit:]c]"""), expected)
  }

  test("regex parsing handles negative digit classes") {
    val negated = notInSet(Diet.one('b') + Range('0', '9') + 'c')

    val expected = lit('a') * negated
    sameRegex(parse("""a[^b\dc]"""), expected)
    sameRegex(parse("""a[^[b[:digit:]c]]"""), expected)
  }

  test("regex parsing handles whitespace classes") {
    val expected = lit('a') * inSet(Diet.fromRange(Range('\t', '\r')) + ' ' + Range('b', 'c'))
    sameRegex(parse("""a[b\sc]"""), expected)
    sameRegex(parse("""a[b[:space:]c]"""), expected)
  }

  test("regex parsing handles negative whitespace classes") {
    val negated = notInSet(CharacterClasses.whitespaceChar + 'b' + 'c')
    val expected = lit('a') * negated
    sameRegex(parse("""a[^b\sc]"""), expected)
    sameRegex(parse("""a[^[b[:space:]c]]"""), expected)
  }

  test("regex parsing handles non-whitespace classes") {
    val expected = lit('a') * Regex.nonWhitespaceChar * lit('c')
    val r = parse("""a\Sc""")
    sameRegex(r, expected)
  }

  test("regex parsing handles non-whitespace classes in negative classes") {
    val expected = lit('a') * Regex.whitespaceChar * lit('c')
    val r = parse("""a[^\S]c""")
    sameRegex(r, expected)
  }

  test("shorthand character classes are intersected in negated character classes") {
    val charClass = inSet(Diet.empty[Char])
    val expected = lit('a') * charClass * lit('c')
    val r = parse("""a[^\s\S]c""")
    sameRegex(r, expected)
  }

  test("negation has higher precedence than intersection") {
    val charClass = inSet(Diet.one('d'))
    val expected = lit('a') * charClass * lit('e')
    val r = parse("""a[^bc&&[bcd]]e""")
    sameRegex(r, expected)
  }

  test("negation has higher precedence than union") {
    val charClass = notInSet(Diet.one('d'))
    val expected = lit('a') * charClass * lit('e')
    val r = parse("""a[^bcd[bc]]e""")
    sameRegex(r, expected)
  }

  test("negation applies to the whole union with brackets") {
    val charClass = notInSet(Diet.fromRange(Range('b', 'f')))
    val expected = lit('a') * charClass * lit('g')
    val r = parse("""a[^[bcd][ef]]g""")
    sameRegex(r, expected)
  }

  test("single literal ampersands are allowed in character classes") {
    val charClass = inSet(Diet.one('b') + '&' + 'c')
    val expected = lit('a') * charClass * lit('d')
    val r = parse("""a[b&c]d""")
    sameRegex(r, expected)
  }

  test("regex parsing handles horizontal whitespace classes") {
    val expected = lit('a') * inSet(Diet.one('\t') + ' ' + 'b' + 'c')
    sameRegex(parse("""a[b\hc]"""), expected)
    sameRegex(parse("""a[b[:blank:]c]"""), expected)
  }

  test("regex parsing handles non-horizontal-whitespace classes") {
    val expected = lit('a') * Regex.nonHorizontalWhitespaceChar * lit('c')
    val r = parse("""a\Hc""")
    sameRegex(r, expected)
  }

  test("regex parsing handles horizontal whitespace classes in a negated character class") {
    val expected = lit('a') * Regex.noneOf('b', '\t', ' ', 'c')
    sameRegex(parse("""a[^b\hc]"""), expected)
    sameRegex(parse("""a[^[b[:blank:]c]]"""), expected)
  }

  test("regex parsing rejects ranges on character class shorthands") {
    assert(parseRegex("""a[b\d-df]""").isLeft)
  }

  test("regex parsing handles ascii classes") {
    val expected = lit('a') * Regex.range('\u0000', '\u007F')
    val r = parse("""a[b[:ascii:]c]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles negative ascii classes") {
    val expected = lit('a') * Regex.nonAsciiChar * lit('c')
    val r = parse("""a[^[:ascii:]]c""")
    sameRegex(r, expected)
  }

  test("regex parsing handles alpha classes") {
    val expected = lit('a') * Regex.alphaChar
    val r = parse("""a[b[:alpha:]c]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles negative alpha classes") {
    val expected = lit('a') * Regex.nonAlphaChar * lit('c')
    val r = parse("""a[^[:alpha:]]c""")
    sameRegex(r, expected)
  }

  test("regex parsing handles alnum classes") {
    val expected = lit('a') * Regex.alphaNumericChar
    val r = parse("""a[b[:alnum:]c]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles negative alnum classes") {
    val expected = lit('a') * Regex.nonAlphaNumericChar * lit('c')
    val r = parse("""a[^[:alnum:]]c""")
    sameRegex(r, expected)
  }

  test("regex parsing handles characters that can only be unescaped inside character classes") {
    val expected = lit('a') * inSet(Diet.one('*') + '<' + '(' + '{' + '|')
    val r = parse("""a[*<({|]""")
    sameRegex(r, expected)
  }

  test(
    "regex parsing handles characters that can only be unescaped inside character classes in negative classes") {
    val expected = lit('a') * Regex.noneOf('*', '<', '(', '{', '|')
    val r = parse("""a[^*<({|]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles character class intersection") {
    val expected = lit('a') * inSet(Diet.one('a'))
    val r = parse("""a[[ab]&&[^b]]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles character class union") {
    val expected = lit('a') * notInSet(CharacterClasses.lowerAlpha - 'a' - 'b')
    val r = parse("""a[[ab][^[:lower:]]]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles character class union/intersection mixes") {
    val expected = lit('a') * Regex.matching(
      MatchSet.allow(CharacterClasses.ascii + 'λ') intersect MatchSet.forbid(
        CharacterClasses.punctuationChar))
    val r = parse("""a[[:ascii:][λ]&&[^[:punct:]]]""")
    sameRegex(r, expected)
  }

  test("character classes allow arbitrary nesting of []") {
    val expected = lit('a') * inSet(Diet.one('b') + 'c')
    val r = parse("""a[[[[b][c]]]]""")
    sameRegex(r, expected)
  }

  test("character classes handle escaped [ and ]") {
    val expected = inSet(Diet.one('[') + 'a' + ']')
    val r = parse("""[\[a\]]""")
    sameRegex(r, expected)
  }

  test("pretty print parser round trip") {
    implicit val regexShrink = RegexShrink.shrinkForRegex[Char]
    forAll(genStandardRegexChar) { r =>
      val clue = s"regex: (${r.pprint})"
      parseRegex(r.pprint) match {
        case Left(label) => withClue(clue)(fail(s"parsing failure: $label"))
        case Right(parsed) => sameRegex(parsed, r)
      }
    }
  }

  test("unicode character points") {
    val r = lit('陸')
    val printed = r.pprint
    printed should ===("\\uf9d3")
    val r2 = parse("\\uf9d3")
    r2.pprint should ===("\\uf9d3")
  }

  test("regex parsing handles empty strings") {
    val expected = Regex.empty[Match[Char]]
    val r = parse("")
    sameRegex(r, expected)
    val matcher = r.stringMatcher
    matcher("") should ===(true)
    matcher("a") should ===(false)
  }

  test("regex parsing handles + matches") {
    val expected = lit('a') * lit('b').oneOrMore * lit('c')
    val r = parse("ab+c")
    sameRegex(r, expected)
  }

  test("regex parsing handles + matches in nested bits") {
    val expected = lit('a') * (lit('b') * lit('c').star).oneOrMore * lit('d')
    val r = parse("a(?:bc*)+d")
    sameRegex(r, expected)
  }

  test("regex parsing handles unicode character points") {
    val expected = Regex.lit('a')
    val r = parse("\\u0061")
    sameRegex(r, expected)
  }

  test("regex parsing handles unicode character points inside ranges") {
    val expected = range('a', 'c')
    val r = parse("[\\u0061-\\u0063]")
    sameRegex(r, expected)
  }

  test("regex parsing only looks at 4 characters for unicode character points") {
    val expected = Regex.lit('a') * lit('1')
    val r = parse("\\u00611")
    sameRegex(r, expected)
  }

  test("regex parsing handles unicode character points inside negated ranges") {
    val expected = notInSet(Diet.fromRange(Range('a', 'c')))
    val r = parse("[^\\u0061-\\u0063]")
    sameRegex(r, expected)
  }

  test("regex parsing handles things that look _almost_ like a POSIX class") {
    sameRegex(parse("[:]"), inSet(Diet.one(':')))

    sameRegex(parse("[:a]"), inSet(Diet.one(':') + 'a'))

    sameRegex(parse("[a:]"), inSet(Diet.one(':') + 'a'))

    sameRegex(parse("[[:]]"), inSet(Diet.one(':')))

    sameRegex(parse("[[:a]]"), inSet(Diet.one(':') + 'a'))

    sameRegex(parse("[[a:]]"), inSet(Diet.one(':') + 'a'))
  }

  test("regex parsing doesn't expect hyphens to be escaped outside of character classes") {
    sameRegex(parse("-"), lit('-'))

    sameRegex(parse("""\d-\d"""), digit * lit('-') * digit)
  }

  test("regex parsing allows escaped hyphens in character classes") {
    sameRegex(parse("""[\-]"""), inSet(Diet.one('-')))
    sameRegex(parse("""[\-a]"""), inSet(Diet.one('-') + 'a'))
    sameRegex(parse("""[a\-]"""), inSet(Diet.one('-') + 'a'))
    sameRegex(parse("""[a\-b]"""), inSet(Diet.one('-') + 'a' + 'b'))
  }

  test("regex parsing fails on invalid regexes") {
    assert(parseRegex("(").isLeft)
    assert(parseRegex(")").isLeft)
    assert(parseRegex("[").isLeft)
    assert(parseRegex("]").isLeft)
    assert(parseRegex("[(").isLeft)
    assert(parseRegex("*").isLeft)
    assert(parseRegex("[a-Z").isLeft)
    assert(parseRegex("a{1,").isLeft)
  }

  def sameRegex(actual: Regex[Char], expected: Regex[Char]): Assertion = {
    val clue =
      s"""(pprint not optimized):
         |    actual: ${actual.pprint}
         |  expected: ${expected.pprint}
         |(pprint optimized):
         |    actual: ${actual.optimize.pprint}
         |  expected: ${expected.optimize.pprint}
         |(structure optimized):
         |    actual: ${actual.optimize}
         |  expected: ${expected.optimize}
         |""".stripMargin
    withClue(clue) {
      // Regex data structures can have structural differences while still being functionally
      // equivalent. For example `Times(x, Times(y, z))` and `Times(Times(x, y), z)`. So we compare
      // them by their pretty-printed equivalence. It's not perfect, but in practice it works pretty
      // well.
      //actual.optimize.pprint should ===(expected.optimize.pprint)
      actual.optimize.pprint should ===(expected.optimize.pprint)
    }
  }

  // TODO ceedubs
  //def sameCapturingRegex(actual: CapturingRegex[Boolean, Char], expected: CapturingRegex[Boolean, Char]): Assertion = {
  //  val clue =
  //    s"""(pprint not optimized):
  //       |    actual: ${actual.pprint}
  //       |  expected: ${expected.pprint}
  //       |(pprint optimized):
  //       |    actual: ${actual.optimize.pprint}
  //       |  expected: ${expected.optimize.pprint}
  //       |(structure optimized):
  //       |    actual: ${actual.optimize}
  //       |  expected: ${expected.optimize}
  //       |""".stripMargin
  //  withClue(clue) {
  //    // Regex data structures can have structural differences while still being functionally
  //    // equivalent. For example `Times(x, Times(y, z))` and `Times(Times(x, y), z)`. So we compare
  //    // them by their pretty-printed equivalence. It's not perfect, but in practice it works pretty
  //    // well.
  //    //actual.optimize.pprint should ===(expected.optimize.pprint)
  //    actual.optimize.pprint should ===(expected.optimize.pprint)
  //  }
  //}
}
