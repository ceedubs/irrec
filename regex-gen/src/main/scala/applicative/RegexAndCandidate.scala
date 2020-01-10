package ceedubs.irrec
package regex.applicative
// TODO package

import ceedubs.irrec.regex.{RegexGen => RegexGenOld}
import ceedubs.irrec.regex.{CharRegexGen => CharRegexGenOld}
import ceedubs.irrec.regex.{RegexMatchGen => RegexMatchGenOld}
import ceedubs.irrec.regex.CharacterClasses
import Regex.Regex
import ceedubs.irrec.regex.Match
import ceedubs.irrec.regex.DietGen.dietMatchingGen

import org.scalacheck.{Arbitrary, Cogen, Gen}
import cats.Order
import cats.implicits._

final case class RegexAndCandidate[In, Out](r: Regex[In, Out], candidate: Stream[In])

object RegexAndCandidate {
  /**
   * Generate a regular expression and a stream that matches the regular expression.
   */
  def genRegexAndMatch[In:Cogen:Order, Out:Arbitrary](
    cfg: RegexGenOld.Config[In],
    matchToGen: Match[In] => Gen[In]): Gen[RegexAndCandidate[In, Out]] =
    for {
      r <- RegexGen.genRegex2(cfg)
      c <- RegexMatchGen.regexMatchingStreamGen(matchToGen)(r)
    } yield RegexAndCandidate(r, c)

    def codyTesting: Gen[List[(String, String, Option[Byte], Regex[Char, Byte])]] = Gen.listOfN(5, genRegexAndMatch[Char, Byte](RegexGenOld.Config.fromDiscreteDiet(CharacterClasses.alphaNumeric), RegexMatchGenOld.dietMatchToGen[Char](CharRegexGenOld.supportedCharacters, dietMatchingGen(_)))).map(_.map{ rc =>
      val compiledR = RE.compile(rc.r)
      (RegexPrettyPrinter.pprintRE(rc.r), rc.candidate.mkString, compiledR.anchoredMatch(rc.candidate), rc.r)
    })
}
