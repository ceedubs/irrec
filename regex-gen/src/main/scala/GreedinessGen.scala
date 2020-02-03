package ceedubs.irrec.regex
package gen

import org.scalacheck.Gen

object GreedinessGen {
  val genGreediness: Gen[Greediness] =
    Gen.frequency(3 -> Greediness.Greedy, 1 -> Greediness.NonGreedy)
}
