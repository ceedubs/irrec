package ceedubs.irrec
package regex

import cats.implicits._
import org.scalacheck.Gen

trait RegexCandidates[In, M] {
  def genMatchingStream[Out](r: Regex[In, M, Out]): Gen[Stream[In]]

  def genCandidateStream[Out](r: Regex[In, M, Out]): Gen[Stream[In]]
}

object RegexCandidates {
  trait GenInRegexCandidates[In, M] extends RegexCandidates[In, M] {
    def genIn: Gen[In]

    override def genCandidateStream[Out](r: Regex[In, M, Out]): Gen[Stream[In]] =
      Gen.oneOf(genMatchingStream(r), Gen.containerOf[Stream, In](genIn))
  }

  // TODO instances for types other than Char
  implicit val regexCCandidates: RegexCandidates[Char, Match[Char]] =
    new GenInRegexCandidates[Char, Match[Char]] {
      def genIn: Gen[Char] = CharRegexGen.genSupportedChars

      def genMatchingStream[Out](r: Regex[Char, Match[Char], Out]): Gen[Stream[Char]] =
        RegexMatchGen
          .dietRegexMatchingStreamGen[Char, Out](CharRegexGen.supportedCharacters)
          .apply(r)
    }
}
