package ceedubs.irrec
package regex

import cats.implicits._
import qq.droste.{Algebra, Gather, RAlgebra, scheme}
import qq.droste.data.CoattrF
import qq.droste.data.prelude._

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

  def kleenePrecedenceAlgebra[A]: Algebra[CoattrF[KleeneF, A, ?], Int] = Algebra{
    CoattrF.un(_) match {
      case Left(_) => 0
      case Right(x) => precedence(x)
    }
  }

  val charsToEscape: Set[Char] = Set('<', '(', '[', '{', '\\', '^', '-', '=', '$', '!', '|', ']',
    '}', ')', '?', '*', '+', '.', '>')

  val charMappings: Map[Char, String] =
    charsToEscape.map(c => (c, "\\" + c.toString)).toMap ++
    Map(
      '\t' -> "\\t",
      '\n' -> "\\n",
      '\r' -> "\\r",
      '\f' -> "\\f")

  val showChar: Char => String = c => charMappings.get(c).getOrElse(c.toString)

  def parensMaybe(currentPrecedence: Int, value: (Int, String), parensForEqualPrecedence: Boolean): String =
    //if (value._1 > currentPrecedence) s"(${value._2})" else value._2
    if (value._1 > currentPrecedence || parensForEqualPrecedence && value._1 === currentPrecedence) s"(${value._2})" else value._2

  def pprintKleene[A]: RAlgebra[Int, KleeneF, String] = RAlgebra[Int, KleeneF, String]{
    case KleeneF.Times(l, r) =>
      parensMaybe(timesPrecedence, l, false) + parensMaybe(timesPrecedence, r, false)
    case KleeneF.Plus(l, r) =>
      s"${parensMaybe(plusPrecedence, l, false)}|${parensMaybe(plusPrecedence, r, false)}"
    case KleeneF.Star(x) => parensMaybe(starPrecedence, x, true) + "*"
    case KleeneF.Zero => "∅"
    case KleeneF.One => ""
  }

  def showMatch[A](f: A => String)(m: Match[A]): String = m match {
    case Match.Literal(a) => f(a)
    case Match.Range(l, r) => s"[${f(l)}-${f(r)}]"
    case Match.Wildcard => "."
  }

  def showCharMatch: Match[Char] => String = showMatch(showChar)

  def pprintCharAlgebra: RAlgebra[Int, CoattrF[KleeneF, Match[Char], ?], String] = RAlgebra{
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
    scheme.gcata(pprintCharAlgebra.gather(
        Gather.zygo(kleenePrecedenceAlgebra[Match[Char]])
    )).apply(r)
}
