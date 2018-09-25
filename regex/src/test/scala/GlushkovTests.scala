package ceedubs.irrec
package regex

import ceedubs.irrec.regex.Regex._

class GlushkovTests extends IrrecSuite {

  test("literal match"){assert(stringMatcher(literal('b'))("b"))}

  test("literal non-match"){assert(!stringMatcher(literal('b'))("a"))}

  test("literal with trailing"){assert(!stringMatcher(literal('b'))("ba"))}

  test("or left match"){assert(stringMatcher(or(literal('b'), literal('c')))("b"))}

  test("or left match with trailing"){assert(!stringMatcher(or(literal('b'), literal('c')))("bc"))}

  test("or right match"){assert(stringMatcher(or(literal('b'), literal('c')))("c"))}

  test("or right match with trailing"){assert(!stringMatcher(or(literal('b'), literal('c')))("cb"))}

  test("or no match"){assert(!stringMatcher(or(literal('b'), literal('c')))("a"))}

  test("or no match with trailing"){assert(!stringMatcher(or(literal('b'), literal('c')))("ad"))}

  test("andThen match"){assert(stringMatcher(andThen(literal('b'), literal('c')))("bc"))}

  test("andThen left only"){assert(!stringMatcher(andThen(literal('b'), literal('c')))("bd"))}

  test("andThen right only"){assert(!stringMatcher(andThen(literal('b'), literal('c')))("ac"))}

  test("andThen with trailing"){assert(!stringMatcher(andThen(literal('b'), literal('c')))("bcd"))}

  test("star zero"){assert(stringMatcher(star(literal('b')))(""))}

  test("star one"){assert(stringMatcher(star(literal('b')))("b"))}

  test("star two"){assert(stringMatcher(star(literal('b')))("bb"))}

  test("star three"){assert(stringMatcher(star(or(literal('b'), literal('c'))))("bcb"))}

  test("star trailing"){assert(!stringMatcher(star(or(literal('b'), literal('c'))))("bcbd"))}

  test("wildcard"){assert(stringMatcher(wildcard[Char])("b"))}

  test("wildcard trailing"){assert(!stringMatcher(wildcard[Char])("bc"))}

  test("wildcard empty"){assert(!stringMatcher(wildcard[Char])(""))}

  test("inside range"){assert(stringMatcher(range('a', 'c'))("b"))}

  test("left range"){assert(stringMatcher(range('a', 'c'))("a"))}

  test("right range"){assert(stringMatcher(range('a', 'c'))("c"))}

  test("outside range"){assert(!stringMatcher(range('a', 'c'))("d"))}

  test("oneOrMore zero"){assert(!stringMatcher(oneOrMore(literal('b')))(""))}

  test("oneOrMore one"){assert(stringMatcher(oneOrMore(literal('b')))("b"))}

  test("oneOrMore two"){assert(stringMatcher(oneOrMore(literal('b')))("bb"))}

  test("oneOrMore three"){assert(stringMatcher(oneOrMore(literal('b')))("bbb"))}

  test("count zero empty"){assert(stringMatcher(count(0, literal('b')))(""))}

  test("count zero non-empty"){assert(!stringMatcher(count(0, literal('b')))("b"))}

  test("count 1 empty"){assert(!stringMatcher(count(1, literal('b')))(""))}

  test("count 1 match"){assert(stringMatcher(count(1, literal('b')))("b"))}

  test("count 1 non-match"){assert(!stringMatcher(count(1, literal('b')))("c"))}

  test("count 2 match"){assert(stringMatcher(count(2, literal('b')))("bb"))}

  test("count 2 non-match"){assert(!stringMatcher(count(2, literal('b')))("bc"))}
}
