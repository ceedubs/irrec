---
id: limitations
title: warnings and limitations
sidebar_label: warnings and limitations
---

## stability

At this point, irrec is just me playing around and learning some things. It provides no stability guarantees.

## limitations

At the moment, irrec does not support the following features:

* **lookarounds** (ex: `(?=...)` for positive lookahead, `?<=...)` for negative lookahead)
* **inline modifiers** (ex: `(?i)` for case-insensitive mode)
* **anchors** (ex: `\b`, `^`, `$`)
* **backreferences** (ex: using `(["'])\w+\1` to capture either a single-quoted or double-quoted string)
* some other odds and ends that are probably mostly straightforward to support if desired

Irrec only checks that an entire string matches a regex, as though the regex had a leading `^` and a trailing `$`.

The wildcard `.` matches on newlines (consistent with a `DOTALL` flag being turned on in some regular expression implementations).

Some of these limitations would probably be pretty easy to remove. Others might be tough to address with the current design of irrec.
