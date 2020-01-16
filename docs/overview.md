---
id: overview
title: overview
sidebar_label: overview
---

Irrec allows you to build, match, and generate regular expressions with a functional API.

The name is a shameless rip-off of [irreg](https://github.com/non/irreg), which this library was inspired by. It's different than irreg in that it uses `rec`ursion schemes, hence the name.

## creating regular expressions

```scala mdoc:silent
import ceedubs.irrec.regex.applicative._, Regex._, char._
import ceedubs.irrec.parse.{regex2 => r}
import ceedubs.irrec.regex.applicative.Greediness._
import cats.implicits._

sealed abstract class Mood
case object Happy extends Mood
case object Tired extends Mood
case object Feisty extends Mood

case class Animals(count: Int, mood: Mood, kind: String)

val animal: Regex[Char, String] = r("(b|c|r|gn)at")
val mood: Regex[Char, Mood] = r("happy").as[Mood](Happy) | r("tired").as(Tired) | r("feisty").as(Feisty)
val animalsR: Regex[Char, Animals] =
  (digit <* horizontalWhitespaceChar.repeat(1, Some(2), Greedy),
  mood <* horizontalWhitespaceChar.repeat(1, Some(2), NonGreedy),
  animal <* lit('s').optional
  ).mapN((count, mood, kind) => Animals(count, mood, kind))
```

```scala mdoc
animalsR.pprint
```

## parsing with a regular expression

```scala mdoc:silent
// TODO help with inference
val animals: ParseState[Char, Animals] = animalsR.compile
```

```scala mdoc
animals.parseOnlyS("1 tired bat")
animals.parseOnlyS("7 feisty cats")
animals.parseOnlyS("3 expensive toasters")
```

## generating data that matches a regular expression

```scala mdoc:silent
import ceedubs.irrec.regex.applicative.CharRegexGen.regexMatchingStringGen
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(animalsR)
```

```scala mdoc
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(79813L))
```
