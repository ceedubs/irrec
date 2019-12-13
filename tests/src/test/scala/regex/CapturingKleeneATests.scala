package ceedubs.irrec
package regex

import Regex._

import cats.data.Chain

class CapturingKleeneATests extends IrrecSuite {
  test("regex capturing") {
    // TODO add some syntax to make this cleaner to define
    val r =
      ((lit('a')).! *> (lit('c') | lit('b')).!, wildcard[Char].star.!, wildcard[Char].!).tupled
        .matcher[List](_.matches(_))
    r(('a' to 'e').toList) should ===(
      Some((Chain.one('b'), Chain.fromSeq('c' to 'd'), Chain.one('e'))))
    r(List('a', 'c', '1')) should ===(Some((Chain.one('c'), Chain.empty, Chain.one('1'))))
    r(('b' to 'e').toList) should ===(None)
    r('a' :: Nil) should ===(None)
  }
}
