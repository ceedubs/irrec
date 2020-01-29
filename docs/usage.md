---
id: usage
title: usage
sidebar_label: examples
---

## creating and matching regular expressions

### string regular expressions

You can create a regular expression via a `String` literal:

```scala mdoc:silent
import ceedubs.irrec.regex._
import ceedubs.irrec.parse.{regex => r}

// a RegexC[A] is a regex that parses a `C`har input sequence into an `A` result
val unitCount: RegexC[Int] = r("""\d{1,3}""").map(_.toInt)
```

You'll even get a compile-time error if the regex is invalid:

```scala mdoc:fail
val invalidUnitCount: RegexC[String] = r("""\d{1,-3}""").map(_.toInt)
```

You can also build a regular expression using the methods in the
`combinator` object, standard [Applicative][Applicative]/[Alternative][Alternative] methods, and irrec's DSL for combining regexes.

* `|` denotes that either the expression on the left _or_ the right needs to match.
* `.star` denotes the Kleene star (repeat 0 to many times).
* [Applicative][Applicative] methods such as `<*`, `*>`, and `mapN` indicate one match followed by another.

```scala mdoc:silent
import ceedubs.irrec.regex.combinator._
import cats.implicits._
import java.time.Duration
import java.time.temporal.ChronoUnit

val chronoUnit: RegexC[ChronoUnit] = r("ms|millis?").as(ChronoUnit.MILLIS) |
  r("s|seconds?").as(ChronoUnit.SECONDS) |
  r("m|mins?|minutes?").as(ChronoUnit.MINUTES)

val duration: RegexC[Duration] = (
  r("-|negative ").optional.map(_.isDefined),
  unitCount <* lit(' ').optional,
  chronoUnit
).mapN{ (isNegative, count, unit) =>
  val d = Duration.of(count.toLong, unit)
  if (isNegative) d.negated else d
}
```

Once you've built a regular expression, you can compile it and parse input with it.

```scala mdoc:silent
val durationParser: ParseState[Char, Duration] = duration.compile
```

```scala mdoc
// parseOnly parses a sequence of input elements
durationParser.parseOnly(Stream('3', 'm', 's'))

// parseOnlyS is specialized to String input
durationParser.parseOnlyS("3ms")

durationParser.parseOnlyS("negative 10 seconds")

durationParser.parseOnlyS("eleventy buckets")
```

## pretty printing

Regular expressions can be printed in a (hopefully) POSIX style:

```scala mdoc
duration.pprint
```

## Java `Pattern`

Regular expressions can be converted to a `java.util.regex.Pattern`:

```scala mdoc
duration.toPattern
```

## generating random data

### random matches for a regular expression

Irrec provides support for creating [Scalacheck](https://www.scalacheck.org/) generators that produce values that match a regular expression. This generation is done efficiently as opposed to generating a bunch of random values and then filtering the ones that don't match the regular expression (which would quickly lead to Scalacheck giving up on generating matching values).

```scala mdoc:silent
import ceedubs.irrec.regex.gen._, CharRegexGen._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

val genDurationString: Gen[String] = genRegexMatchingString(duration)
```

```scala mdoc
Gen.listOfN(3, genDurationString).apply(Gen.Parameters.default, Seed(1046531L))
```

Sometimes you may want to generate both matches and non-matches for your regular expression to make sure that both cases are handled. There are various `Gen` instances that will generate input that matches the regular expresssion roughly half of the time.

```scala mdoc:silent
val genDurationCandidateString: Gen[String] =
  Gen.resize(12, genRegexCandidateString(duration))

val genExamples: Gen[List[(String, Option[Duration])]] =
  Gen.listOfN(3, genDurationCandidateString).map(candidates =>
    candidates.map(candidate => (candidate, durationParser.parseOnlyS(candidate)))
  )
```

```scala mdoc
genExamples.apply(Gen.Parameters.default, Seed(1046531L))
```

### random regular expressions

Irrec provies support for creating random (valid) regular expressions along with potential matches for them.

```scala mdoc:silent
val regexGen: Gen[RegexC[List[Long]]] = Gen.resize(12, genAsciiRegex)

val randomRegex1: RegexC[List[Long]] = regexGen.apply(Gen.Parameters.default, Seed(105769L)).get
```

```scala mdoc
randomRegex1.pprint
```

You can now generate random data to match this regular expression as described [here](#random-matches-for-a-regular-expression). Alternatively, you can generate a regular expression and a match for it in one step:

```scala mdoc:silent
val regexAndMatchGen: Gen[RegexAndCandidate[Char, Double]] =
  Gen.resize(12, genAlphaNumRegexAndMatch)

val regexesAndMatchesGen: Gen[List[RegexAndCandidate[Char, Double]]] =
  Gen.listOfN(4, regexAndMatchGen)

val regexesAndMatches: List[RegexAndCandidate[Char, Double]] = regexesAndMatchesGen.apply(Gen.Parameters.default.withSize(30), Seed(105773L)).get
```

```scala mdoc
regexesAndMatches.map(x =>
  (x.r.pprint, x.candidate.mkString)
)
```

Sometimes you may want to generate both matches and non-matches for your random regular expression to make sure that both cases are handled. There are various `Gen` instances for `RegexAndCandidate` that will generate random regular expressions along with data that matches the regular expresssion roughly half of the time.

```scala mdoc:silent
val regexAndCandidateGen: Gen[RegexAndCandidate[Char, Double]] =
  Gen.resize(12, genAlphaNumRegexAndCandidate)

val regexesAndCandidatesGen: Gen[List[RegexAndCandidate[Char, Double]]] =
  Gen.listOfN(4, regexAndCandidateGen)

val regexesAndCandidates: List[RegexAndCandidate[Char, Double]] = regexesAndCandidatesGen.apply(Gen.Parameters.default.withSize(30), Seed(105771L)).get
```

```scala mdoc
regexesAndCandidates.map(x =>
  (x.r.pprint, x.candidate.mkString, x.r.compile.parseOnly(x.candidate))
)
```

### non-string regular expressions

While `RegexC` is the most common choice, irrec supports regular expressions for types other than chars/strings. For example if your input is a stream of integers instead of a string:


```scala mdoc:silent
import Greediness._

val numRegex: RegexM[Int, Unit] = lit(1).many.void <* range(2, 4).repeat(1, Some(3), Greedy) <* oneOf(5, 6).oneOrMore(Greedy)

val numMatcher: Stream[Int] => Boolean = numRegex.matcher[Stream]
```

```scala mdoc
numMatcher(Stream(1, 2, 5))

numMatcher(Stream(1, 1, 1, 2, 4, 5, 6, 5))

numMatcher(Stream(0, 5, 42))
```

The `M` in `RegexM` stands for [Match](https://ceedubs.github.io/irrec/api/ceedubs/irrec/regex/Match.html). It is useful for matches on any discrete input type such as characters or numbers.

[Applicative]: https://typelevel.org/cats/typeclasses/applicative.html
[Alternative]: https://typelevel.org/cats/typeclasses/alternative.html
