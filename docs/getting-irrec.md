---
id: getting-irrec
title: getting irrec
---

If you are using SBT, you can add irrec as a dependency to your project with:

```scala
libraryDependencies ++= Seq(
  // for basic functionality
  "@ORG@" %% "irrec-parser" % "@VERSION@",
  // for Scalacheck generators
  "@ORG@" %% "irrec-regex-gen" % "@VERSION@" % Test
)
```

In addition to bringing in core functionality, the `irrec-parser` module provides support for creating regexes from strings. If you are okay with inheriting a [fastparse](http://www.lihaoyi.com/fastparse/) dependency, then it's probably the way to go. If you don't want to inherit this dependency and plan to create regexes via the DSL, then you can depend on `iirec-regex`.

### Scala.js

Irrec is cross-published for Scala.js, and you can depend on it from your Scala.js project in the usual way (use `%%%` instead of `%%` when specifying the dependency).
