---
id: limitations
title: warnings and limitations
sidebar_label: warnings and limitations
---

## stability

At this point, irrec is just me playing around and learning some things. It provides no stability guarantees.

## limitations

At the moment, irrec does not support the following features:

* **capturing and backreferences** (ex: using `hello, (\w+)` and capturing the string that matched `\w+`)
* **lookarounds** (ex: `(?=...)` for positive lookahead, `?<=...)` for negative lookahead)
* **non-greedy matches** (ex: `.*?`)
* **inline modifiers** (ex: `(?i)` for case-insensitive mode)
* **anchors** (ex: `\b`, `^`, `$`)
* some other odds and ends that are probably mostly straightforward to support if desired

Irrec only checks that an entire string matches a regex, as though the regex had a leading `^` and a trailing `$`.

The wildcard `.` matches on newlines (consistent with a `DOTALL` flag being turned on in some regular expression implementations).

Some of these limitations would probably be pretty easy to remove. Others might be tough to address with the current design of irrec.
