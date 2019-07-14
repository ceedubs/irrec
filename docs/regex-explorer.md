---
id: regex-explorer
title: regular expression explorer
sidebar_label: regex explorer
---

Since irrec cross-compiles with [Scala.js](https://www.scala-js.org/), it can be used with consistent APIs and results on both the JVM and in the browser. Try out some regular expressions below! Irrec will parse your regular expression, compile it, match your _candidate_ against it, and even generate some random matches for your regular expression.

```scala mdoc:js:invisible
<ul class="key-value">
<li>regex: <input type="text" name="regex" size="76" value="It's been [2-9]{1,3} (hours|days) since the last (injury|NPE|\ud83d\udc0a)\."/><p class="regex-error-msg"></p></li>
<li>candidate: <input type="text" name="match" size="60" value="It's been 7 hours since the last injury."/></li>
<li>RNG seed: <input type="text" name="rng-seed" size="10" value="21"/></li>

</ul>

Here are some strings that match your regular expression:
<ul class="regex-matches"></ul>

<div class="nfa-viz"></div>
---

{
import ceedubs.irrec.regex._
import ceedubs.irrec.parse.Parser
import RegexPrettyPrinter.showCharMatch
import org.scalajs.dom
import org.scalajs.dom.html.Input
import fastparse.Parsed.{Failure, Success}
import ceedubs.irrec.regex.CharRegexGen.regexMatchingStringGen
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import cats.implicits._
import scala.scalajs.js

val matchInput = node.querySelector("""input[name="match"]""").asInstanceOf[Input]

val regexInput = node.querySelector("""input[name="regex"]""").asInstanceOf[Input]

val seedInput = node.querySelector("""input[name="rng-seed"]""").asInstanceOf[Input]

val regexErrorText = node.querySelector("p.regex-error-msg")

val regexMatchesList = node.querySelector("ul.regex-matches")

val nfaViz = node.querySelector("div.nfa-viz")

def genMatches(r: Regex[Char], seed: Seed): List[String] = {
  val matchGen = regexMatchingStringGen(r)
  Gen.listOfN(30, matchGen)
  .map(_.distinct.take(5))
  .apply(Gen.Parameters.default, seed)
  .getOrElse(List.empty)
}

def update(): Unit = {
  val regexValue = regexInput.value
  val matchValue = matchInput.value
  val seed = Either.catchNonFatal(seedInput.value.toLong)
    .fold(
      { _ =>
        seedInput.classList.add("invalid")
        Seed(1046527L)
      },
      { s =>
        seedInput.classList.remove("invalid")
        Seed(s)
      })
  regexMatchesList.innerHTML = ""
  fastparse.parse(regexValue, Parser.regexExpr(_), verboseFailures = true) match {
    case f @ Failure(_, _, _) =>
      regexInput.classList.add("invalid")
      regexErrorText.textContent = s"Error compiling regular expression: ${f.msg}"
    case Success(r, _) =>
      regexInput.classList.remove("invalid")
      regexErrorText.textContent = ""
      val isMatch = r.stringMatcher(matchValue)
      if (isMatch) matchInput.classList.remove("invalid") else matchInput.classList.add("invalid")
      genMatches(r, seed).foreach{ s =>
        val li = dom.document.createElement("li")
        li.textContent = s
        regexMatchesList.appendChild(li)
      }
      val _ = js.Dynamic.global.cytoscape(nfaGraphDict(Glushkov.kleeneToNFA(r)))
  }
}

def nfaGraphDict(nfa: NFA[Int, Match[Char]]): js.Dictionary[js.Any] = {
  val (nodes, transitions) = nfa.transitions.toList.foldMap{ case (node1, transitions) =>
    transitions.foldMap{ case (node2, m) =>
      (Set(node2), List((node1, node2, showCharMatch(m))))
    } |+| ((Set(node1), List.empty))
  } |+| ((nfa.initStates ++ nfa.finalStates, List.empty))

  val nodeDicts = nodes.toList.map{ node =>
    js.Dictionary[js.Any]("data" -> js.Dictionary[js.Any](
      "id" -> node,
      "initState" -> nfa.initStates.contains(node),
      "finalState" -> nfa.finalStates.contains(node)))
  }
  val edgeDicts = transitions.map{ case (node1, node2, m) =>
    js.Dictionary("data" ->
      js.Dictionary[js.Any](
        "id" -> s"$node1->$node2",
        "source" -> node1,
        "target" -> node2,
        "label" -> m))
  }

  js.Dictionary(
    "container" -> nfaViz,
    "userZoomingEnabled" -> false,
    "layout" -> js.Dictionary[js.Any](
      "name" -> "breadthfirst",
      "directed" -> true,
      "grid" -> false,
      "maximal" -> true,
      "roots" -> js.Array(nfa.initStates.toList: _*),
      "animate" -> false),
    "style" -> js.Array(
      js.Dictionary(
        "selector" -> "edge",
        "style" -> js.Dictionary[js.Any](
          "curve-style" -> "bezier",
          "width" -> "1px",
          "target-arrow-shape" -> "triangle",
          "label" -> "data(label)",
          "text-background-shape" -> "roundrectangle",
          "text-background-padding" -> "3px",
          "text-background-color" -> "#17B890",
          "text-background-opacity" -> 1)),
      js.Dictionary(
        "selector" -> "node[!initState, !finalState]",
        "style" -> js.Dictionary[js.Any](
          "shape" -> "ellipse",
          "height" -> "1px",
          "width" -> "1px")),
      js.Dictionary(
        "selector" -> "node[?finalState]",
        "style" -> js.Dictionary[js.Any](
          "shape" -> "star",
          "height" -> "14px",
          "width" -> "14px")),
      js.Dictionary(
        "selector" -> "node[?initState]",
        "style" -> js.Dictionary[js.Any](
          "shape" -> "vee",
          "width" -> "14px",
          "height" -> "14px"))),
    "elements" -> js.Dictionary(
      "nodes" -> js.Array(nodeDicts: _*),
      "edges" -> js.Array(edgeDicts: _*)))
}

// start things off
update()

matchInput.oninput = { _ =>
  update()
}

regexInput.oninput = { _ =>
  update()
}

seedInput.oninput = { _ =>
  update()
}

}
```
