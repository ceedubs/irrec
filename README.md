# irrec

An implementation of regular expressions based on recursion schemes and Kleene algebras.

## warning

At this point, this library is just me playing around and learning some things. It provides no stability guarantees, and I don't know if I'll ever even get around to publishing it. That said, if you are interested in actually using it, please let me know! If enough people star this repository maybe I'll do something with it.

## creating and matching a string regular expression

```scala
import ceedubs.irrec.regex._, Regex._

val animal: Regex[Char] = (oneOf(lit('b'), lit('c'), lit('r')) | allOf(lit('g'), lit('n'))) * lit('a') * lit('t')

// a more concise way to write the same expression
val animal: Regex[Char] = (oneOfL('b', 'c', 'r') | seqL("gn")) * seqL("at")

//  way of writing this

val isAnimal: String => Boolean = animal.stringMatcher
```

```scala
isAnimal("bat")
// res3: Boolean = true

isAnimal("cat")
// res4: Boolean = true

isAnimal("rat")
// res5: Boolean = true

isAnimal("gnat")
// res6: Boolean = true

isAnimal("hat")
// res7: Boolean = false

isAnimal("toaster")
// res8: Boolean = false
```

The `L` in `OneOfL`, `allOfL`, and `seqL` means "literal".

## generating data that matches a regular expression

Irrec provides support for creating [Scalacheck](https://www.scalacheck.org/) generators that produce values that match a regular expression. This generation is done efficiently as opposed to generating a bunch of random values and then filtering the ones that don't match the regular expression (which would quickly lead to Scalacheck giving up on generating matching values).

```scala
val n: Regex[Char] = range('2', '9')
val adjective: Regex[Char] = oneOf(seqL("happy"), seqL("tired"), seqL("feisty"))
val phrase: Regex[Char] = n * lit(' ') * adjective * lit(' ') * animal * lit('s')
```

```scala
import ceedubs.irrec.regex.RegexGen.regexMatchingStringGen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.rng.Seed

val phraseGen: Gen[String] = regexMatchingStringGen(phrase, arbitrary[Char])

val p: Gen.Parameters = Gen.Parameters.default
```

```scala
Gen.listOfN(3, phraseGen).apply(p, Seed(105769L))
// res9: Option[List[String]] = Some(List(5 tired rats, 2 feisty gnats, 8 happy bats))
```

## Inspiration and Credits

A number of libraries and resources were useful as inspiration and reference implementations for the code in this library. A special thanks goes out to these:

- [irreg](https://github.com/non/irreg) by [Erik Osheim](https://github.com/non). Irrec is inspired by irreg and Erik's talk [Regexes, Kleene Algebras, and Real Ultimate Power!](https://vimeo.com/96644096)
- [Extending Glushkov NFA with sub matching over Strings](http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html), a blog post by Kenny Zhuo Ming Lu. The implementation of the Glushkov construction algorithm in irrec is based on the Haskell implementation in this blog post.
- [Andy Scott](https://github.com/andyscott) has been helpful both in creating [droste](https://github.com/andyscott/droste) (the recursion scheme library that irrec uses), and in answering my questions about recursion schemes.
