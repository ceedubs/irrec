package ceedubs.irrec
package regex
package applicative

import ceedubs.irrec.regex.Regex._

// TODO
class RETests extends IrrecSuite {
  test("literal match") { assert(literal('b').stringMatcher("b")) }
}
