package ceedubs.irrec
package regex

import CharRegexGen._

class RegexPrettyPrinterJvmTests extends IrrecSuite {
  test("char regex pretty printer matches java Pattern") {
    forAll(genSupportedRegexAndMatch[Unit]) { rm =>
      val prettyRegex = rm.r.pprint
      val regexHex = prettyRegex.map(_.toInt.toHexString).toList
      val javaR = rm.r.toPattern
      val candidateHex = rm.candidate.map(_.toInt.toHexString).toList
      assert(
        javaR.matcher(rm.candidate.mkString).matches,
        s"${rm.candidate.mkString} ($candidateHex) should match $prettyRegex ($regexHex)")
    }
  }
}
