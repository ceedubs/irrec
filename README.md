# irrec

An implementation of regular expressions based on recursion schemes and Kleene algebras.

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
// res1: Boolean = true

isAnimal("cat")
// res2: Boolean = true

isAnimal("rat")
// res3: Boolean = true

isAnimal("gnat")
// res4: Boolean = true

isAnimal("hat")
// res5: Boolean = false

isAnimal("toaster")
// res6: Boolean = false
```

## generating data that matches a regular expression

Irrec provides support for creating [Scalacheck](https://www.scalacheck.org/) generators that produce values that match a regular expression. This generation is done efficiently as opposed to generating a bunch of random values and then filtering the ones that don't match the regular expression (which would quickly lead to Scalacheck giving up on generating matching values).

```scala
val n: Regex[Char] = range('2', '9')
val adjective: Regex[Char] = oneOfR(seq("happy"), seq("tired"), seq("feisty"))
val phrase: Regex[Char] = n * lit(' ') * adjective * lit(' ') * animal * lit('s')
```

```scala
import ceedubs.irrec.regex.RegexGen.regexMatchingStringGen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(phrase, arbitrary[Char])
```

```scala
Gen.listOfN(3, phraseGen).apply(Gen.Parameters.default, Seed(105769L))
// res7: Option[List[String]] = Some(List(5 tired rats, 2 feisty gnats, 8 happy bats))
```

## Inspiration and Credits

A number of libraries and resources were useful as inspiration and reference implementations for the code in this library. A special thanks goes out to these:

- [irreg](https://github.com/non/irreg) by [Erik Osheim](https://github.com/non). Irrec is inspired by irreg and Erik's talk [Regexes, Kleene Algebras, and Real Ultimate Power!](https://vimeo.com/96644096)
- [Extending Glushkov NFA with sub matching over Strings](http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html), a blog post by Kenny Zhuo Ming Lu. The implementation of the Glushkov construction algorithm in irrec is based on the Haskell implementation in this blog post.
- [Andy Scott](https://github.com/andyscott) has been helpful both in creating [droste](https://github.com/andyscott/droste) (the recursion scheme library that irrec uses), and in answering my questions about recursion schemes.
