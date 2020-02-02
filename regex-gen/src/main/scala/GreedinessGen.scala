package ceedubs.irrec.regex
package gen

import org.scalacheck.Gen

object GreedinessGen {
  val genGreediness: Gen[Greediness] = Gen.oneOf(Greediness.Greedy, Greediness.NonGreedy)
}
