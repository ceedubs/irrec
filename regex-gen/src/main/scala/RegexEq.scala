package ceedubs.irrec
package regex

import cats.implicits._
import cats.{~>, Eq}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

/**
 * This defines "equality" for regular expressions based on whether they produce the same result for
 * a random sampling of inputs. This is a somewhat dodgy thing to do and should be used with
 * caution.
 */
object RegexEq {
  private val genSeed: Gen[Seed] = Gen.choose(Long.MinValue, Long.MaxValue).map(Seed(_))

  def genRegexEq[In, M](
    implicit rc: RegexCandidates[In, M]): Gen[Eq ~> λ[a => Eq[Regex[In, M, a]]]] =
    Gen.parameterized { params =>
      genSeed.map { seed =>
        new (Eq ~> λ[a => Eq[Regex[In, M, a]]]) {
          def apply[Out](eqOut: Eq[Out]): Eq[Regex[In, M, Out]] = {
            implicit val impEqOut = eqOut
            Eq.instance { (r1, r2) =>
              val candidateGen = Gen.oneOf(rc.genCandidateStream(r1), rc.genCandidateStream(r2))
              val candidates =
                Gen.listOfN(params.size, candidateGen)(params, seed).getOrElse(List.empty)
              candidates.forall { candidate =>
                r1.compile.parseOnly(candidate) === (r2.compile.parseOnly(candidate))
              }
            }
          }
        }
      }
    }
}
