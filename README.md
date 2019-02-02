# irrec

[![Build Status](https://travis-ci.org/ceedubs/irrec.svg?branch=master)](https://travis-ci.org/ceedubs/irrec/branches)
[![codecov.io](http://codecov.io/github/ceedubs/irrec/coverage.svg?branch=master)](http://codecov.io/github/ceedubs/irrec?branch=master)

An implementation of regular expressions based on recursion schemes and Kleene algebras.

The name is a shameless rip-off of [irreg](https://github.com/non/irreg), which this library was inspired by. It's different than irreg in that it uses `rec`ursion schemes, hence the name.

## warning

At this point, this library is just me playing around and learning some things. It provides no stability guarantees, and I don't know if I'll ever even get around to publishing it. That said, if you are interested in actually using it, please let me know! If enough people star this repository maybe I'll do something with it.

## creating and matching a string regular expression

```scala
import ceedubs.irrec.regex._, Regex._

// `*` denotes that the expression on the right should follow the expression on the left.
val animal: Regex[Char] = (oneOf('b', 'c', 'r') | seq("gn")) * seq("at")

val isAnimal: String => Boolean = animal.stringMatcher
```

```scala
isAnimal("bat")
// res0: Boolean = true

isAnimal("cat")
// res1: Boolean = true

isAnimal("rat")
// res2: Boolean = true

isAnimal("gnat")
// res3: Boolean = true

isAnimal("hat")
// res4: Boolean = false

isAnimal("toaster")
// res5: Boolean = false
```

## creating and matching a non-string regular expression

While `Regex[Char]` is the most common choice, irrec supports regular expressions for types other than chars/strings. For example if your input is a stream of integers instead of a string:


```scala
// needed for Foldable[Stream] instance
import cats.implicits._

val numRegex: Regex[Int] = lit(1).star * range(2, 4).repeat(1, 3) * oneOf(5, 6).oneOrMore

val numMatcher: Stream[Int] => Boolean = numRegex.matcher[Stream]
```

```scala
numMatcher(Stream(1, 2, 5))
// res6: Boolean = true

numMatcher(Stream(1, 1, 1, 2, 4, 5, 6, 5))
// res7: Boolean = true

numMatcher(Stream(0, 5, 42))
// res8: Boolean = false
```

## printing a regular expression

Regular expressions can be printed in a (hopefully) POSIX style:

```scala
animal.pprint
// res9: String = "(b|c|r|gn)at"
```

## converting a regular expression to a Java `Pattern`

Regular expressions can be converted to a `java.util.regex.Pattern`:

```scala
animal.toPattern
// res10: java.util.regex.Pattern = (b|c|r|gn)at
```

Currently there is no support for converting a `Pattern` (or its `String` form) into an irrec `Regex`.

## generating data that matches a regular expression

Irrec provides support for creating [Scalacheck](https://www.scalacheck.org/) generators that produce values that match a regular expression. This generation is done efficiently as opposed to generating a bunch of random values and then filtering the ones that don't match the regular expression (which would quickly lead to Scalacheck giving up on generating matching values).

```scala
val n: Regex[Char] = range('2', '9')
val adjective: Regex[Char] = oneOfR(seq("happy"), seq("tired"), seq("feisty"))
val phrase: Regex[Char] = n * lit(' ') * adjective * lit(' ') * animal * lit('s')
```

```scala
import ceedubs.irrec.regex.RegexGen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(arbitrary[Char])(phrase)
```

```scala
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(105769L))
// res11: Option[List[String]] = Some(
//   List("5 tired rats", "2 feisty gnats", "8 happy bats")
// )
```

## generating random regular expressions

Irrec provies support for creating random (valid) regular expressions along with potential matches for them.

```scala
val regexGen: Gen[Regex[Char]] = arbitrary[Regex[Char]]

val randomRegex1: Regex[Char] = regexGen.apply(Gen.Parameters.default, Seed(105769L)).get
```

```scala
randomRegex1.pprint
// res12: String = "(\u245b|[\ua615-\uc4eb]\u64db)*.*([\uce76-\ud24f]\uaefa\u1363\u881f*|([\ud4b2-\ud618]|\u6ba1)\u7009)([\u8086-\ud694]\ube8e|.)"
```

You can now generate random data to match this regular expression as described [here](#generating-data-that-matches-a-regular-expression). Alternatively, you can generate a regular expression and a match for it in one step:

```scala
val regexAndMatchGen: Gen[RegexAndCandidate[Char]] =
  CharRegexGen.genAlphaNumCharRegexAndMatch

val regexesAndMatchesGen: Gen[List[RegexAndCandidate[Char]]] =
  Gen.listOfN(4, regexAndMatchGen)

val regexesAndMatches: List[RegexAndCandidate[Char]] = regexesAndMatchesGen.apply(Gen.Parameters.default.withSize(30), Seed(105773L)).get
```

```scala
regexesAndMatches.map(x =>
  (x.r.pprint, x.candidate.mkString)
)
// res13: List[(String, String)] = List(
//   (".S[o-x]", "pSx"),
//   ("7(Y|[e-k])u", "7iu"),
//   ("sbb0(s|[7-n]*|[l-t]s).B.", "sbb0rszBG"),
//   ("[a-j]", "i")
// )
```

Sometimes you may want to generate both matches and non-matches for your random regular expression to make sure that both cases are handled. There are various `Gen` instances for `RegexAndCandidate` that will generate random regular expressions along with data that matches the regular expresssion roughly half of the time.

```scala
val regexAndCandidateGen: Gen[RegexAndCandidate[Char]] =
  CharRegexGen.genAlphaNumCharRegexAndCandidate

val regexesAndCandidatesGen: Gen[List[RegexAndCandidate[Char]]] =
  Gen.listOfN(4, regexAndCandidateGen)

val regexesAndCandidates: List[RegexAndCandidate[Char]] = regexesAndCandidatesGen.apply(Gen.Parameters.default.withSize(30), Seed(105771L)).get
```

```scala
regexesAndCandidates.map(x =>
  (x.r.pprint, x.candidate.mkString, x.r.matcher[Stream].apply(x.candidate))
)
// res14: List[(String, String, Boolean)] = List(
//   ("i*[e-r].[L-m][7-c]rqzakh[q-x][C-p][8-w]", "riK8acx3d", false),
//   ("zhh(9z|z)[i-l].*", "zhhzklpawcbbw", true),
//   ("m*.*.[7-d]", "mmmmmmmmmmmmmmmmmmmmmmmmm5thgxjucrTaA", true),
//   ("[2-a]|i", "hb1anjz1afksdbtsunyahpqNrjDahd", false)
// )
```

## optimizing a regular expression

Irrec has some support for optimizing a regular expression, though at this point it probably won't
do much to optimize most regular expressions.

```scala
val inefficientRegex: Regex[Char] = lit('a').star.star.star
```

```scala
inefficientRegex.pprint
// res15: String = "((a*)*)*"
```

```scala
val moreEfficientRegex: Regex[Char] = inefficientRegex.optimize
```

```scala
moreEfficientRegex.pprint
// res16: String = "a*"
```

## performance

Irrec has been built with algorithmic performance in mind but at this point, it isn't built to be blazingly fast. It is built with a focus on correctness and clean (by some standard) functional code.

Some benchmark results can be viewed in the [benchmarks/results](benchmarks/results) directory. In a sentence (that is a really lossy representation of reality), for common use-cases Java's `Pattern` performs about an order of magnitude better than irrec (think 10M matches per second vs 1M). However for some extreme cases, irrec performs several orders of magnitude better (think 500k matches per second vs 30).

## inspiration and credits

A number of libraries and resources were useful as inspiration and reference implementations for the code in this library. A special thanks goes out to these:

- [irreg](https://github.com/non/irreg) by [Erik Osheim](https://github.com/non). Irrec is inspired by irreg and Erik's talk [Regexes, Kleene Algebras, and Real Ultimate Power!](https://vimeo.com/96644096)
- [Extending Glushkov NFA with sub matching over Strings](http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html), a blog post by Kenny Zhuo Ming Lu. The implementation of the Glushkov construction algorithm in irrec is based on the Haskell implementation in this blog post.
- [Andy Scott](https://github.com/andyscott) has been helpful both in creating [droste](https://github.com/andyscott/droste) (the recursion scheme library that irrec uses), and in answering my questions about recursion schemes.

