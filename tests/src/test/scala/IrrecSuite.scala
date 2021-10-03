package ceedubs.irrec

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.typelevel.discipline.scalatest.FunSuiteDiscipline
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalactic.anyvals.{PosInt, PosZDouble, PosZInt}
import org.typelevel.discipline.Laws
import org.scalacheck.Prop

abstract class IrrecSuite
    extends AnyFunSuite
    with Matchers
    with ScalaCheckPropertyChecks
    with FunSuiteDiscipline
    with StrictCatsEquality {

  lazy val checkConfiguration: PropertyCheckConfiguration =
    PropertyCheckConfiguration(
      minSuccessful = PosInt(10),
      maxDiscardedFactor = PosZDouble(5.0),
      minSize = PosZInt(0),
      sizeRange = PosZInt(10),
      workers = PosInt(2))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    checkConfiguration

  def rulesetToProp(rules: Laws#RuleSet): Prop = {
    val props = rules.all.properties.map { case (id, prop) =>
      prop.label(id)
    }
    Prop.all(props.toSeq: _*)
  }
}
