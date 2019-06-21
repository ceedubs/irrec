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
import ceedubs.irrec.parse.regex

val animalLit: Regex[Char] = regex("(b|c|r|gn)at")
```

You'll even get a compile-time error if the regex is invalid:

```scala mdoc:fail
val invalid: Regex[Char] = regex("a{1,-3}")
```

Alternatively, you can build up a regular expression using the methods in the
`Regex` object and irrec's DSL for combining regexes.

* `*` denotes that the expression on the right should follow the expression on the left.
* `+` denotes that either the expression on the left _or_ the right needs to match.
* `.star` denotes the Kleene star (repeat 0 to many times).

```scala mdoc:silent
import ceedubs.irrec.regex.Regex._

val animalDSL: Regex[Char] = (oneOf('b', 'c', 'r') | seq("gn")) * seq("at")
```

Whether you have created a `Regex` via a `String` literal or the DSL, irrec's
regular expressions are composable.

```scala mdoc:silent
val count: Regex[Char] = range('2', '9')
val adjective: Regex[Char] = regex("happy|tired|feisty")
val animalPhrase: Regex[Char] = count * lit(' ') * adjective * lit(' ') * animalDSL * lit('s')
```

```scala mdoc
animalPhrase.pprint
```

### non-string regular expressions

While `Regex[Char]` is the most common choice, irrec supports regular expressions for types other than chars/strings. For example if your input is a stream of integers instead of a string:


```scala mdoc:silent
// needed for Foldable[Stream] instance
import cats.implicits._

val numRegex: Regex[Int] = lit(1).star * range(2, 4).repeat(1, Some(3)) * oneOf(5, 6).oneOrMore

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
import ceedubs.irrec.regex.RegexGen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(arbitrary[Char])(animalPhrase)
```

```scala mdoc
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(1046527L))
```

### random regular expressions

Irrec provies support for creating random (valid) regular expressions along with potential matches for them.

```scala mdoc:silent
val regexGen: Gen[Regex[Char]] = arbitrary[Regex[Char]]

val randomRegex1: Regex[Char] = regexGen.apply(Gen.Parameters.default, Seed(105769L)).get
```

```scala mdoc
randomRegex1.pprint
```

You can now generate random data to match this regular expression as described [here](#random-matches-for-a-regular-expression). Alternatively, you can generate a regular expression and a match for it in one step:

```scala mdoc:silent
val regexAndMatchGen: Gen[RegexAndCandidate[Char]] =
  CharRegexGen.genAlphaNumCharRegexAndMatch

val regexesAndMatchesGen: Gen[List[RegexAndCandidate[Char]]] =
  Gen.listOfN(4, regexAndMatchGen)

val regexesAndMatches: List[RegexAndCandidate[Char]] = regexesAndMatchesGen.apply(Gen.Parameters.default.withSize(30), Seed(105773L)).get
```

```scala mdoc
regexesAndMatches.map(x =>
  (x.r.pprint, x.candidate.mkString)
)
```

Sometimes you may want to generate both matches and non-matches for your random regular expression to make sure that both cases are handled. There are various `Gen` instances for `RegexAndCandidate` that will generate random regular expressions along with data that matches the regular expresssion roughly half of the time.

```scala mdoc:silent
val regexAndCandidateGen: Gen[RegexAndCandidate[Char]] =
  CharRegexGen.genAlphaNumCharRegexAndCandidate

val regexesAndCandidatesGen: Gen[List[RegexAndCandidate[Char]]] =
  Gen.listOfN(4, regexAndCandidateGen)

val regexesAndCandidates: List[RegexAndCandidate[Char]] = regexesAndCandidatesGen.apply(Gen.Parameters.default.withSize(30), Seed(105771L)).get
```

```scala mdoc
regexesAndCandidates.map(x =>
  (x.r.pprint, x.candidate.mkString, x.r.matcher[Stream].apply(x.candidate))
)
```
