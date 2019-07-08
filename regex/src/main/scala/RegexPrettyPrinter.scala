package ceedubs.irrec
package regex

import cats.Eq
import cats.implicits._
import qq.droste.{scheme, Algebra, Gather, RAlgebra}
import qq.droste.data.CoattrF
import qq.droste.data.prelude._
import cats.collections.Diet

object RegexPrettyPrinter {
  private val timesPrecedence: Int = 2
  private val plusPrecedence: Int = 3
  private val starPrecedence: Int = 1
  private val onePrecedence: Int = 0
  private val zeroPrecedence: Int = 0

  val precedence: KleeneF[_] => Int = {
    case KleeneF.Times(_, _) => timesPrecedence
    case KleeneF.Plus(_, _) => plusPrecedence
    case KleeneF.Star(_) => starPrecedence
    case KleeneF.One => onePrecedence
    case KleeneF.Zero => zeroPrecedence
  }

  def kleenePrecedenceAlgebra[A]: Algebra[CoattrF[KleeneF, A, ?], Int] = Algebra {
    CoattrF.un(_) match {
      case Left(_) => 0
      case Right(x) => precedence(x)
    }
  }

  val nonCharClassCharsToEscape: Set[Char] = Set('<', '(', '[', '{', '\\', '^', '-', '=', '$', '!',
    '|', ']', '}', ')', '?', '*', '+', '.', '>')

  val charClassCharsToEscape: Set[Char] = Set(']', '\\', '^', '-')

  val whitespaceCharMappings: Map[Char, Char] =
    Map('t' -> '\t', 'n' -> '\n', 'r' -> '\r', 'f' -> '\f')

  val specialNonCharClassCharToLit: Map[Char, Char] =
    whitespaceCharMappings ++ nonCharClassCharsToEscape.map(x => (x, x))

  val specialCharClassCharToLit: Map[Char, Char] =
    whitespaceCharMappings ++ charClassCharsToEscape.map(x => (x, x))

  val nonCharClassCharToEscapedChar: Map[Char, String] =
    specialNonCharClassCharToLit.map {
      case (special, lit) =>
        (lit, "\\" + special)
    }

  val charClassCharToEscapedChar: Map[Char, String] =
    specialCharClassCharToLit.map {
      case (special, lit) =>
        (lit, "\\" + special)
    }

  // TODO ceedubs should output \r in a way that doesn't cause newlines
  def showChar(inCharacterClass: Boolean): Char => String =
    if (inCharacterClass) c => charClassCharToEscapedChar.get(c).getOrElse(c.toString)
    else c => nonCharClassCharToEscapedChar.get(c).getOrElse(c.toString)

  def parensMaybe(
    currentPrecedence: Int,
    value: (Int, String),
    parensForEqualPrecedence: Boolean): String =
    if (value._1 > currentPrecedence || parensForEqualPrecedence && value._1 === currentPrecedence)
      s"(${value._2})"
    else value._2

  def pprintKleene[A]: RAlgebra[Int, KleeneF, String] = RAlgebra[Int, KleeneF, String] {
    case KleeneF.Times(l, r) =>
      parensMaybe(timesPrecedence, l, false) + parensMaybe(timesPrecedence, r, false)
    case KleeneF.Plus(l, r) =>
      s"${parensMaybe(plusPrecedence, l, false)}|${parensMaybe(plusPrecedence, r, false)}"
    case KleeneF.Star(x) => parensMaybe(starPrecedence, x, true) + "*"
    // this is kind of hacky, but it seems unlikely that someone will use Zero in a regex and care
    // about how it prints.
    case KleeneF.Zero => "∅"
    case KleeneF.One => ""
  }

  /**
   * @param f a function that takes an `A` value and a boolean indicating whether or not the `A` is
   * appearing within a range and formats it as a string.
   */
  def showMatch[A](f: (Boolean, A) => String)(implicit eqA: Eq[A]): Match[A] => String = {
    import Match._

    def showDiet(diet: Diet[A]): String =
      diet.foldLeftRange("") {
        case (s, cats.collections.Range(l, h)) =>
          val current = if (l === h) f(true, l) else s"${f(true, l)}-${f(true, h)}"
          s + current
      }

    _ match {
      case Literal(a) => f(false, a)
      case MatchSet.Allow(allowed) =>
        if (allowed.isEmpty) pprintKleene(KleeneF.Zero) else s"[${showDiet(allowed)}]"
      case MatchSet.Forbid(forbidden) =>
        if (forbidden.isEmpty) "." else s"[^${showDiet(forbidden)}]"
      case Match.Wildcard() => "."
    }
  }

  def showCharMatch: Match[Char] => String = showMatch((inRange, c) => showChar(inRange)(c))

  def pprintCharAlgebra: RAlgebra[Int, CoattrF[KleeneF, Match[Char], ?], String] = RAlgebra {
    CoattrF.un(_) match {
      case Left(m) => showCharMatch(m)
      case Right(ks) => pprintKleene(ks)
    }
  }

  /**
   * Print a regex in POSIX style.
   *
   * NOTE: irrec regular expressions are allowed to contain patterns such as `(b*)*`
   */
  def pprintCharRegex(r: Regex[Char]): String =
    scheme
      .gcata(
        pprintCharAlgebra.gather(
          Gather.zygo(kleenePrecedenceAlgebra[Match[Char]])
        ))
      .apply(r)
}
