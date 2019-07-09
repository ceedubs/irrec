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
---

{
import ceedubs.irrec.regex._
import ceedubs.irrec.parse.Parser
import org.scalajs.dom
import org.scalajs.dom.html.Input
import fastparse.Parsed.{Failure, Success}
import ceedubs.irrec.regex.CharRegexGen.regexMatchingStringGen
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import cats.implicits._

val matchInput = node.querySelector("""input[name="match"]""").asInstanceOf[Input]

val regexInput = node.querySelector("""input[name="regex"]""").asInstanceOf[Input]

val seedInput = node.querySelector("""input[name="rng-seed"]""").asInstanceOf[Input]

val regexErrorText = node.querySelector("p.regex-error-msg")

val regexMatchesList = node.querySelector("ul.regex-matches")

def genMatches(r: Regex[Char], seed: Seed): List[String] = {
  val matchGen = regexMatchingStringGen(r)
  Gen.listOfN(20, matchGen)
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
  }
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
