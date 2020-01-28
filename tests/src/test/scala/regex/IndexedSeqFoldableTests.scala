package ceedubs.irrec
package regex

import cats.{Eq, Foldable}
import cats.implicits._
import cats.laws.discipline.{FoldableTests, SerializableTests}

class IndexedSeqFoldableTests extends IrrecSuite {
  {
    implicit val indexedSeqFoldable: Foldable[IndexedSeq] = new IndexedSeqFoldable[IndexedSeq] {}
    implicit val indexedSeqIntEq: Eq[IndexedSeq[Int]] = Eq.fromUniversalEquals
    checkAll("IndexedSeq", FoldableTests[IndexedSeq].foldable[Int, Int])
    checkAll("Foldable[IndexedSeq]", SerializableTests.serializable(Foldable[IndexedSeq]))
  }
}
