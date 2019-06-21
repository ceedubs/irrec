---
id: overview
title: overview
sidebar_label: overview
---

Irrec allows you to build, match, and generate regular expressions with a functional API.

The name is a shameless rip-off of [irreg](https://github.com/non/irreg), which this library was inspired by. It's different than irreg in that it uses `rec`ursion schemes, hence the name.

## creating regular expressions

```scala mdoc:silent
import ceedubs.irrec.regex._, Regex._
import ceedubs.irrec.parse.regex

val animal: Regex[Char] = regex("(b|c|r|gn)at")
val phrase: Regex[Char] = regex("[2-9] (happy|tired|feisty) ") * animal * lit('s')
```

```scala mdoc
phrase.pprint
```

## matching against a regular expression

```scala mdoc:silent
val matchesPhrase: String => Boolean = phrase.stringMatcher
```

```scala mdoc
matchesPhrase("7 feisty cats")
matchesPhrase("3 expensive toasters")
```

## generating data that matches a regular expression

```scala mdoc:silent
import ceedubs.irrec.regex.RegexGen._
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(arbitrary[Char])(phrase)
```

```scala mdoc
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(79817L))
```
