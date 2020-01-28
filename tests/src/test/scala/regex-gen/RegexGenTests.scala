package ceedubs.irrec
package regex

import RegexGen.Support._

import cats.implicits._
import org.scalacheck.Gen

class RegexGenTests extends IrrecSuite {

  test("distributeSumNel fails on entryCount that is too small") {
    val gen = for {
      entryCount <- Gen.chooseNum(Int.MinValue, 0)
      extra <- Gen.chooseNum(0, 100)
      ints <- distributeSumNel(entryCount, extra)
    } yield ints

    gen.sample should ===(None)
  }

  test("ambGenConversion1 should error if used") {
    Either.catchNonFatal(ambGenConversion1(1)).bimap(_.getMessage, _ => ()) should ===(
      Left("🍩 use implicit Gen conversion: 1"))
  }

  test("ambGenConversion2 should error if used") {
    Either.catchNonFatal(ambGenConversion2(2)).bimap(_.getMessage, _ => ()) should ===(
      Left("🍩 use implicit Gen conversion: 2"))
  }
}
