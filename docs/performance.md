---
id: performance
title: performance
---

## optimizing a regular expression

Irrec has some support for optimizing a regular expression, though at this point it probably won't
do much to optimize most regular expressions.

```scala mdoc:silent
import ceedubs.irrec.regex._, Regex._

val inefficientRegex: Regex[Char] = lit('a').star.star.star
```

```scala mdoc
inefficientRegex.pprint
```

```scala mdoc:silent
val moreEfficientRegex: Regex[Char] = inefficientRegex.optimize
```

```scala mdoc
moreEfficientRegex.pprint
```

## performance

Irrec has been built with algorithmic performance in mind but at this point, it isn't built to be blazingly fast. It is built with a focus on correctness and clean (by some standard) functional code.

Some benchmark results can be viewed in the [benchmarks/results](https://github.com/ceedubs/irrec/tree/master/benchmarks/results) directory. In a sentence (that is a really lossy representation of reality), for common use-cases Java's `Pattern` performs about an order of magnitude better than irrec (think 10M matches per second vs 1M). However for some extreme cases, irrec performs several orders of magnitude better (think 500k matches per second vs 30).

