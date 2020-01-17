package ceedubs.irrec
package parse

import Parser.parseRegex

import java.util.regex.Pattern

/**
 * Java and JavaScript have some differing behaviors when it comes to regular expressions. In fact,
 * the behavior of JavaScript regular expressions varies from one JS engine to another. One way that
 * this manifests is in character class matches. For example the `null` character (`\u0000`) matches
 * the `\p{ASCII}` POSIX character class in Java, but it does not in the node.js regular expression
 * engine. For now we work around this by only testing irrec's behavior as consistent with the JVM
 * implementation.
 */
class ParserJvmTests extends IrrecSuite {
  import ParserJvmTests._

  test("POSIX positive class consistency with Pattern") {
    forAll { c: Char =>
      val s = c.toString
      posixClassNames foreach { className =>
        val javaClass = posixClassToJavaClass(className)
        val clue = s"posix class: $className, candidate: (${c.toInt.toHexString})"
        val irrecRegex = s"""[a[:$className:]c]"""
        val pattern = Pattern.compile(s"""^[a${javaClass}c]$$""", Pattern.DOTALL)
        parseRegex(irrecRegex) match {
          case Left(err) => withClue(clue)(fail(s"parsing failure: $err"))
          case Right(parsed) =>
            withClue(clue)(parsed.stringMatcher.apply(s) should ===(pattern.matcher(s).matches))
        }
      }
    }
  }

  test("POSIX negative class consistency with Pattern") {
    forAll { c: Char =>
      val s = c.toString
      posixClassNames foreach { className =>
        val javaClass = posixClassToJavaClass(className)
        val clue = s"posix class: $className, candidate: (${c.toInt.toHexString})"
        val irrecRegex = s"""[^[:$className:]]"""
        val pattern = Pattern.compile(s"""^[^${javaClass}]$$""", Pattern.DOTALL)
        parseRegex(irrecRegex) match {
          case Left(err) => withClue(clue)(fail(s"parsing failure: $err"))
          case Right(parsed) =>
            withClue(clue)(parsed.stringMatcher.apply(s) should ===(pattern.matcher(s).matches))
        }
      }
    }
  }
}

object ParserJvmTests {
  val posixClassNames: Set[String] = Set(
    "alnum",
    "alpha",
    "ascii",
    "blank",
    "cntrl",
    "digit",
    "graph",
    "lower",
    "print",
    "punct",
    "space",
    "upper",
    "word",
    "xdigit")

  def posixClassToJavaClass(posixClassName: String): String = posixClassName match {
    case "word" => "\\w"
    case "ascii" => "\\p{ASCII}"
    case "xdigit" => "\\p{XDigit}"
    case s =>
      val (firstChar, rest) = s.splitAt(1)
      s"\\p{${firstChar.toUpperCase}$rest}"
  }
}
