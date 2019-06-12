package ceedubs.irrec
package parse

import ceedubs.irrec.regex._
import CharRegexGen._
import ceedubs.irrec.parse.{regex => parse}

import qq.droste.data.Coattr
import cats.data.NonEmptyList
import fastparse._
import ceedubs.irrec.regex.Regex._
import org.scalatest.compatible.Assertion

class ParserTests extends IrrecSuite {
  def parseRegex(regex: String): Parsed[Regex[Char]] = fastparse.parse(regex, Parser.regexExpr(_))

  test("regex parsing works for single literal"){
    val expected = Regex.lit('a')
    val r = parse("a")
    sameRegex(r, expected)
  }

  test("regex parsing works for literal then literal"){
    val expected = Regex.lit('a') * Regex.lit('b')
    val r = parse("ab")
    sameRegex(r, expected)
  }
  test("regex parsing works for literal or literal"){
    val expected = Regex.lit('a') | Regex.lit('b')
    val r = parse("a|b")
    sameRegex(r, expected)
  }

  test("regex parsing matches disjunction of literal sequences"){
    val expected = Regex.seq("ab") | Regex.seq("bc")
    val r = parse("ab|bc")
    sameRegex(r, expected)
  }

  test("regex parsing matches unnecessary outer parens"){
    val expected = Regex.seq("ab")
    val r = parse("(ab)")
    sameRegex(r, expected)
  }

  test("regex parsing handles non-capturing parens"){
    val expected = lit('a') * seq("bc") * lit('d')
    val r = parse("a(?:bc)d")
    sameRegex(r, expected)
  }

  test("regex parsing matches literal*"){
    val expected = Regex.lit('a').star
    val r = parse("a*")
    sameRegex(r, expected)
  }

  test("regex parsing matches literal* then another matcher"){
    val expected = Regex.lit('a').star * lit('b')
    val r = parse("a*b")
    sameRegex(r, expected)
  }

  test("regex parsing handles precedence with *"){
    val expected = Regex.lit('a') | (Regex.lit('c') * Regex.lit('d').star)
    val r = parse("a|cd*")
    sameRegex(r, expected)
  }

  test("regex parsing respects parens"){
    val expected = (Regex.lit('a') | Regex.lit('c')) * Regex.lit('d')
    val r = parse("(a|c)d")
    sameRegex(r, expected)
  }

  test("regex parsing is fine with nested parens"){
    val expected = (Regex.lit('a') | Regex.lit('c')) * Regex.lit('d')
    val r = parse("(((a|(c)))d)")
    sameRegex(r, expected)
  }

  test("regex parsing respects spaces"){
    val expected = seq("ab cd")
    val r = parse("ab cd")
    sameRegex(r, expected)
  }

  test("regex parsing respects tabs"){
    val expected = seq("ab\tcd")
    val r = parse("ab\tcd")
    sameRegex(r, expected)
  }

  test("regex parsing supports character classes"){
    val expected = lit('a') * Regex.oneOf('b', 'c', 'd') * lit('e')
    val r = parse("a[bcd]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges"){
    val expected = lit('a') * range('b', 'd') * lit('e')
    val r = parse("a[b-d]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges with multiple ranges and non-ranges"){
    val expected = lit('a') * (range('b', 'd') | Regex.oneOf('e', 'g') | range('i', 'k')) * lit('e')
    val r = parse("a[b-degi-k]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports ranges with negative character classes"){
    import Match.Negated._
    import Match.{Range, Literal}

    val negated: Regex[Char] = Coattr.pure(
      Match.NoneOf(
        NonEmptyList.of(
          NegatedRange(Range('b', 'd')),
          NegatedLiteral(Literal('e')),
          NegatedLiteral(Literal('g')),
          NegatedRange(Range('i', 'k')))))

    val expected = lit('a') * negated * lit('e')

    val r = parse("a[^b-degi-k]e")
    sameRegex(r, expected)
  }

  test("regex parsing supports exact repeat counts"){
    val expected = lit('a') * lit('b').repeat(3, Some(3)) * lit('e')
    val r = parse("ab{3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges starting with 1"){
    val expected = lit('a') * lit('b').repeat(1, Some(3)) * lit('e')
    val r = parse("ab{1,3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges starting with 0"){
    val expected = lit('a') * lit('b').repeat(0, Some(3)) * lit('e')
    val r = parse("ab{0,3}e")
    sameRegex(r, expected)
  }

  test("regex parsing supports count ranges with unbounded upper limit"){
    val expected = lit('a') * lit('b').repeat(1, None) * lit('e')
    val r = parse("ab{1,}e")
    sameRegex(r, expected)
  }

  test("regex parsing handles complex nested expressions"){
    val expected = (lit('a') | (lit('b') * wildcard.star)) * lit('d')
    val r = parse("(a|b.*)d")
    sameRegex(r, expected)
  }

  test("regex parsing handles digit classes"){
    val expected = lit('a') * (lit('b') | Regex.digit | lit('c'))
    val r = parse("""a[b\dc]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles negative digit classes"){
    import Match.{lit => _, _}
    val expected = lit('a') * Coattr.pure(
      NoneOf(
        NonEmptyList.of(
          Negated.NegatedLiteral(Literal('b')),
          Negated.NegatedRange(Range('0','9')),
          Negated.NegatedLiteral(Literal('c')))))
    val r = parse("""a[^b\dc]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles whitespace classes"){
    val expected = lit('a') * (lit('b') | Regex.whitespaceCharacter | lit('c'))
    val r = parse("""a[b\sc]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles negative whitespace classes"){
    import Match.{lit => _, _}
    val expected = lit('a') * Coattr.pure(
      NoneOf(
      NonEmptyList.of(
        Negated.NegatedLiteral(Literal('b')),
        Negated.NegatedLiteral(Literal('\t')),
        Negated.NegatedLiteral(Literal('\n')),
        Negated.NegatedLiteral(Literal('\f')),
        Negated.NegatedLiteral(Literal('\r')),
        Negated.NegatedLiteral(Literal(' ')),
        Negated.NegatedLiteral(Literal('c')))))
    val r = parse("""a[^b\sc]""")
    sameRegex(r, expected)
  }

  test("regex parsing handles non-whitespace classes"){
    val expected = lit('a') * Regex.nonWhitespaceCharacter * lit('c')
    val r = parse("""a\Sc""")
    sameRegex(r, expected)
  }

  test("regex parsing rejects ranges on character class shorthands"){
    assert(!parseRegex("""a[b\d-df]""").isSuccess)
  }

  test("pretty print parser round trip"){
    forAll(genCharRegexAndCandidate){ case RegexAndCandidate(r, s) =>
      withClue(s"regex: (${r.pprint}), candidate: (${s.mkString})"){
        val Parsed.Success(parsed, _) = parseRegex(r.pprint)
        sameRegex(parsed, r)
        r.matcher[Stream].apply(s) should ===(parsed.matcher[Stream].apply(s))
      }
    }
  }

  test("regex parsing handles empty strings"){
    val expected = Regex.empty[Match[Char]]
    val r = parse("")
    sameRegex(r, expected)
    val matcher = r.stringMatcher
    matcher("") should ===(true)
    matcher("a") should ===(false)
  }

  test("regex parsing handles + matches"){
    val expected = lit('a') * lit('b').oneOrMore * lit('c')
    val r = parse("ab+c")
    sameRegex(r, expected)
  }

  test("regex parsing handles + matches in nested bits"){
    val expected = lit('a') * (lit('b') * lit('c').star).oneOrMore * lit('d')
    val r = parse("a(bc*)+d")
    sameRegex(r, expected)
  }

  test("regex parsing fails on invalid regexes"){
    assert(!parseRegex("(").isSuccess)
    assert(!parseRegex(")").isSuccess)
    assert(!parseRegex("[").isSuccess)
    assert(!parseRegex("]").isSuccess)
    assert(!parseRegex("[(").isSuccess)
    assert(!parseRegex("*").isSuccess)
    assert(!parseRegex("[a-Z").isSuccess)
    assert(!parseRegex("a{1,").isSuccess)
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
    withClue(clue){
      // Regex data structures can have structural differences while still being functionally
      // equivalent. For example `Times(x, Times(y, z))` and `Times(Times(x, y), z)`. So we compare
      // them by their pretty-printed equivalence. It's not perfect, but in practice it works pretty
      // well.
      actual.optimize.pprint should ===(expected.optimize.pprint)
    }
  }
}
