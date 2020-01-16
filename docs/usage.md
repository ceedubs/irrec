---
id: usage
title: usage
sidebar_label: examples
---

## creating and matching regular expressions

### string regular expressions

You can create a regular expression via a `String` literal:

```scala mdoc:silent
import ceedubs.irrec.regex.applicative._, Regex._
import ceedubs.irrec.parse.{regex2 => r}

val animalLit: RegexC[String] = r("(b|c|r|gn)at")
```

You'll even get a compile-time error if the regex is invalid:

```scala mdoc:fail
val invalid: RegexC[String] = r("a{1,-3}")
```

Alternatively, you can build up a regular expression using the methods in the
`Regex` object, standard Applicative/Alternative methods, and irrec's DSL for combining regexes.

* `|` denotes that either the expression on the left _or_ the right needs to match.
* `.star` denotes the Kleene star (repeat 0 to many times).
* Applicative methods such as `<*`, `*>`, and `mapN` indicate one match followed by another.

```scala mdoc:silent
import ceedubs.irrec.regex.applicative.Regex._
import cats.implicits._

val animalDSL: RegexC[Unit] = (oneOf('b', 'c', 'r').void | seq("gn").void) <* seq("at")
val animalDSLWithMatched: RegexC[String] = animalDSL.matchedS
```

Whether you have created a `Regex` via a `String` literal or the DSL, irrec's
regular expressions are composable.

```scala mdoc:silent
val count: RegexC[Char] = range('2', '9')
val adjective: RegexC[String] = r("happy|tired|feisty")
val animalPhrase: RegexC[String] = (
  count <* lit(' '),
  adjective <* lit(' '),
  animalDSL,
  lit('s').optional
).mapN((c, adj, a, s) => s"""$c $adj $a${s.getOrElse("")}""")
```

```scala mdoc
animalPhrase.pprint
```

### non-string regular expressions

While `RegexC` is the most common choice, irrec supports regular expressions for types other than chars/strings. For example if your input is a stream of integers instead of a string:


```scala mdoc:silent
import Greediness._

val numRegex: Regex[Int, Unit] = lit(1).many.void <* range(2, 4).repeat(1, Some(3), Greedy) <* oneOf(5, 6).oneOrMore(Greedy)

val numMatcher: Stream[Int] => Boolean = numRegex.matcher[Stream]
```

```scala mdoc
numMatcher(Stream(1, 2, 5))

numMatcher(Stream(1, 1, 1, 2, 4, 5, 6, 5))

numMatcher(Stream(0, 5, 42))
```

## pretty printing

Regular expressions can be printed in a (hopefully) POSIX style:

```scala mdoc
animalLit.pprint
```

## Java `Pattern`

Regular expressions can be converted to a `java.util.regex.Pattern`:

```scala mdoc
animalLit.toPattern
```

## generating random data

### random matches for a regular expression

Irrec provides support for creating [Scalacheck](https://www.scalacheck.org/) generators that produce values that match a regular expression. This generation is done efficiently as opposed to generating a bunch of random values and then filtering the ones that don't match the regular expression (which would quickly lead to Scalacheck giving up on generating matching values).

```scala mdoc:silent
import ceedubs.irrec.regex.applicative.CharRegexGen._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(animalPhrase)
```

```scala mdoc
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(1046525L))
```

### random regular expressions

Irrec provies support for creating random (valid) regular expressions along with potential matches for them.

```scala mdoc:silent
val regexGen: Gen[Regex[Char, List[Long]]] = Gen.resize(12, CharRegexGen.genAsciiRegex)

val randomRegex1: Regex[Char, List[Long]] = regexGen.apply(Gen.Parameters.default, Seed(105769L)).get
```

```scala mdoc
randomRegex1.pprint
```

You can now generate random data to match this regular expression as described [here](#random-matches-for-a-regular-expression). Alternatively, you can generate a regular expression and a match for it in one step:

```scala mdoc:silent
val regexAndMatchGen: Gen[RegexAndCandidate[Char, Double]] =
  Gen.resize(12, CharRegexGen.genAlphaNumRegexAndMatch)

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
  Gen.resize(12, CharRegexGen.genAlphaNumRegexAndCandidate)

val regexesAndCandidatesGen: Gen[List[RegexAndCandidate[Char, Double]]] =
  Gen.listOfN(4, regexAndCandidateGen)

val regexesAndCandidates: List[RegexAndCandidate[Char, Double]] = regexesAndCandidatesGen.apply(Gen.Parameters.default.withSize(30), Seed(105771L)).get
```

```scala mdoc
regexesAndCandidates.map(x =>
  (x.r.pprint, x.candidate.mkString, x.r.matcher[Stream].apply(x.candidate))
)
```
