---
id: overview
title: overview
sidebar_label: overview
---

Irrec allows you to build, match, and generate regular expressions with a functional API.

The name is a shameless rip-off of [irreg](https://github.com/non/irreg), which this library was inspired by. The `rec` in `irrec` originall came from `rec`ursion schemes. The current version does not use recursion schemes and is based off of the Haskell [regex-applicative](https://hackage.haskell.org/package/regex-applicative) library.

## creating regular expressions

```scala mdoc:silent
import ceedubs.irrec.regex._, combinator._, char._
import ceedubs.irrec.parse.{regex => r}
import ceedubs.irrec.regex.Greediness._
import cats.implicits._

sealed abstract class Mood
case object Happy extends Mood
case object Tired extends Mood
case object Feisty extends Mood

case class Animals(count: Int, mood: Mood, kind: String)

// RegexC is a regular expression that matches against a sequence of input `C`haracters
val animal: RegexC[String] = r("(b|c|r|gn)at")

val mood: RegexC[Mood] = r("happy").as[Mood](Happy) | r("tired").as(Tired) |
  r("feisty").as(Feisty)

val animalsR: RegexC[Animals] =
  (digit <* horizontalWhitespaceChar.repeat(1, Some(2), Greedy),
  mood <* horizontalWhitespaceChar.repeat(1, Some(2), NonGreedy),
  animal <* lit('s').optional
  ).mapN(Animals.apply)
```

```scala mdoc
animalsR.pprint
```

## parsing with a regular expression

```scala mdoc:silent
val animals: ParseState[Char, Animals] = animalsR.compile
```

```scala mdoc
animals.parseOnlyS("1 tired bat")
animals.parseOnlyS("7 feisty cats")
animals.parseOnlyS("3 expensive toasters")
```

## generating data that matches a regular expression

```scala mdoc:silent
import ceedubs.irrec.regex.gen.CharRegexGen.genRegexMatchingString
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = genRegexMatchingString(animalsR)
```

```scala mdoc
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(79813L))
```
