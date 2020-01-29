package ceedubs.irrec
package regex

import ceedubs.irrec.parse.{regex => parse}

import cats.implicits._

class ParseStateTests extends IrrecSuite {
  test("parseOnly stops consuming input if there is no possible match") {
    val r = parse("(a|b)c*")
    val input = Stream('f', 'g').append(sys.error("this shouldn't happen..."))
    // shouldn't throw an exception
    r.compile.parseOnly(input) should ===(None)
  }
}
