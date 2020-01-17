---
id: credits
title: inspiration and credits
sidebar_label: inspiration and credits
---

A number of libraries and resources were useful as inspiration and reference implementations for the code in this library. A special thanks goes out to these:

- [regex-applicative](https://hackage.haskell.org/package/regex-applicative). The current version of irrec is largely a port of regex-applicative from Haskell to Scala.
- [irreg](https://github.com/non/irreg) by [Erik Osheim](https://github.com/non). Irrec is inspired by irreg and Erik's talk [Regexes, Kleene Algebras, and Real Ultimate Power!](https://vimeo.com/96644096)
- [Extending Glushkov NFA with sub matching over Strings](http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html), a blog post by Kenny Zhuo Ming Lu. The implementation of the Glushkov construction algorithm in irrec is based on the Haskell implementation in this blog post.
- [Andy Scott](https://github.com/andyscott) has been helpful both in creating [droste](https://github.com/andyscott/droste) (the recursion scheme library that irrec uses), and in answering my questions about recursion schemes.
- [Parsing regular expressions with recursive descent](http://matt.might.net/articles/parsing-regex-with-recursive-descent/). The grammar for regular expressions that this article presents helped me to greatly simplify the regex parsing code in irrec.
- [mdoc](https://scalameta.org/mdoc/)'s features and documentation were incredibly useful in creating a website with compiler-checked code examples.
