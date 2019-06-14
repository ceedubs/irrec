package ceedubs.irrec
package regex

import cats.{Foldable, Order, Reducible}
import cats.implicits._
import qq.droste.data.Coattr
import cats.data.NonEmptyList

// TODO ceedubs work around that scala bug where a companion object and type alias have the same name
object Regex {

  /** alias for [[literal]] */
  def lit[A](value: A): Regex[A] = literal(value)

  def literal[A](value: A): Regex[A] = Coattr.pure(Match.Literal(value))

  def range[A](l: A, r: A): Regex[A] = Coattr.pure(Match.Range(l, r))

  def wildcard[A]: Regex[A] = Coattr.pure(Match.Wildcard)

  def or[A](l: Kleene[A], r: Kleene[A]): Kleene[A] = Coattr.roll(KleeneF.Plus(l, r))

  def andThen[A](l: Kleene[A], r: Kleene[A]): Kleene[A] = Coattr.roll(KleeneF.Times(l, r))

  def oneOf[A](a1: A, as: A*): Regex[A] = as.foldLeft(lit(a1))((acc, a) => or(acc, lit(a)))

  def oneOfR[A](r1: Kleene[A], rs: Kleene[A]*): Kleene[A] = rs.foldLeft(r1)((acc, r) => or(acc, r))

  def oneOfF[F[_], A](values: F[A])(implicit reducibleF: Reducible[F]): Regex[A] =
    reducibleF.reduceLeftTo(values)(lit(_))((acc, a) => or(acc, literal(a)))

  def oneOfFR[F[_], A](values: F[Kleene[A]])(implicit reducibleF: Reducible[F]): Kleene[A] =
    reducibleF.reduceLeft(values)((acc, r) => or(acc, r))

  /**
   * AKA `+` in regular expressions, but I avoided confusion with `Plus` corresponding to "or".
   */
  def oneOrMore[A](value: Kleene[A]): Kleene[A] = andThen(value, star(value))

  def star[A](value: Kleene[A]): Kleene[A] = Coattr.roll(KleeneF.Star(value))

  def allOfFR[F[_], A](values: F[Kleene[A]])(implicit foldableF: Foldable[F]): Kleene[A] =
    foldableF.foldLeft(values, empty[A])((acc, a) => andThen(acc, a))

  def allOfF[F[_], A](values: F[A])(implicit foldableF: Foldable[F]): Regex[A] =
    foldableF.foldLeft(values, empty[Match[A]])((acc, a) => andThen(acc, literal(a)))

  def allOfR[A](values: Kleene[A]*): Kleene[A] =
    allOfFR(values.toList)

  def allOf[A](values: A*): Regex[A] =
    allOfF(values.toList)

  def seq[A](values: Seq[A]): Regex[A] =
    values.foldLeft(empty[Match[A]])((acc, a) => andThen(acc, literal(a)))

  def repeat[A](minInclusive: Int, maxInclusive: Option[Int], r: Kleene[A]): Kleene[A] =
    count(minInclusive, r) * maxInclusive.fold(star(r))(max =>
      (1 to (max - minInclusive)).foldLeft(empty[A])((acc, i) => or(acc, r.count(i))))

  def optional[A](r: Kleene[A]): Kleene[A] =
    r | empty

  /**
   * A match on the empty string (this should always succeed and consume no input).
   */
  def empty[A]: Kleene[A] = Coattr.roll[KleeneF, A](KleeneF.One)

  /**
   * A regular expression that will never successfully match.
   *
   * This is part of all Kleene algebras but may not be particularly useful in the context of
   * string/character regexes.
   */
  def impossible[A]: Kleene[A] = Coattr.roll[KleeneF, A](KleeneF.Zero)

  def count[A](n: Int, r: Kleene[A]): Kleene[A] =
    (1 to n).foldLeft(empty[A])((acc, _) => andThen(acc, r))

  /**
   * Matches a single digit character ('0', '3', '9', etc). Could be represented in a regular
   * expression as `\d` or `[0-9]`.
   */
  val digit: Regex[Char] = Coattr.pure(CharacterClasses.digitMatch)

  /**
   * Opposite of [[digit]]. Could be represented in a regular expression as
   * `\D`.
   */
  val nonDigit: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonDigitMatch)))

  /**
   * Matches a single lowercase character ('a', 'z', etc). Could be represented in a regular
   * expression as `[a-z]` or `[[:lower:]]`.
   */
  val lowerAlphaChar: Regex[Char] = Coattr.pure(CharacterClasses.lowerAlphaMatch)

  /**
   * Opposite of [[lowerAlphaChar]]. Could be represented in a regular expression as
   * `[^a-z]` or `[^[:lower:]]`.
   */
  val nonLowerAlphaChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonLowerAlphaMatch)))

  /**
   * Matches a single uppercase character ('a', 'z', etc). Could be represented in a regular
   * expression as `[a-z]` or `[[:upper:]]`.
   */
  val upperAlphaChar: Regex[Char] = Coattr.pure(CharacterClasses.upperAlphaMatch)

  /**
   * Opposite of [[upperAlphaChar]]. Could be represented in a regular expression as
   * `[^a-z]` or `[^[:upper:]]`.
   */
  val nonUpperAlphaChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonUpperAlphaMatch)))

  /**
   * Matches a single alphabetic character ('a', 'A', etc). Could be represented in a regular
   * expression as `[[:alpha:]]`.
   */
  val alphaChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.alphaMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[alphaChar]]. Could be represented in a regular expression as
   * `[^[:alalpha:]]`.
   */
  val nonAlphaChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonAlphaMatches))

  /**
   * Matches a single alphanumeric character ('0', 'a', 'A', etc). Could be represented in a regular
   * expression as `[[:alnum:]]`.
   */
  val alphaNumericChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.alphaNumericMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[alphaNumericChar]]. Could be represented in a regular expression as
   * `[^[:alnum:]]`.
   */
  val nonAlphaNumericChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonAlphaNumericMatches))

  /**
   * Matches a single hexadecimal digit ('0', '1', 'A', 'F', 'a', 'f', etc). Could be represented in
   * a regular expression as `[[:xdigit:]]`.
   */
  val hexDigitChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.hexDigitMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[hexDigitChar]]. Could be represented in a regular expression as
   * `[^[:alnum:]]`.
   */
  val nonHexDigitChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonHexDigitMatches))

  /**
   * Matches a single "word" character ('A', 'a', '_', etc). Could be represented in a regular
   * expression as `\w`.
   */
  val wordChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.wordCharMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[wordChar]]. Could be represented in a regular expression as
   * `\W`.
   */
  val nonWordChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonWordCharMatches))

  /**
   * A single horizontal whitespace character `[\t ]`. Could be represented in a regular expression
   * as `\h`.
   */
  val horizontalWhitespaceChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.horizontalWhitespaceCharMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[horizontalWhitespaceChar]]; this matches on any character that is not a tab
   * or a space. Could be represented in a regular expression as `\H`.
   */
  val nonHorizontalWhitespaceChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonHorizontalWhitespaceCharMatches))

  /**
   * A single whitespace character `[\t\n\f\r ]`. Could be represented in a regular expression as
   * `\s`.
   */
  val whitespaceChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.whitespaceCharMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[whitespaceChar]]. Could be represented in a regular expression as
   * `\S`.
   */
  val nonWhitespaceChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonWhitespaceCharMatches))

  /**
   * A single ASCII character `[ -~]`. Could be represented in a regular expression as
   * `[[:ascii:]]`.
   */
  val asciiChar: Regex[Char] = Coattr.pure(CharacterClasses.asciiMatch)

  /**
   * Opposite of [[asciiChar]]. Could be represented in a regular expression as
   * `[^[:ascii:]]`.
   */
  val nonAsciiChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonAsciiMatch)))

  /**
   * A single control character `[\x00-\x1F\x7F]`. Could be represented in a regular expression as
   * `[[:cntrl:]]`.
   */
  val controlChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.controlCharMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[controlChar]]. Could be represented in a regular expression as
   * `[^[:cntrl:]]`.
   */
  val nonControlChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonControlCharMatches))

  /**
   * A single visible (graphical) character `[\x21-\x7E]`. Could be represented in a regular
   * expression as `[[:graph:]]`.
   */
  val graphChar: Regex[Char] = Coattr.pure(CharacterClasses.graphCharMatch)

  /**
   * Opposite of [[graphChar]]. Could be represented in a regular expression as `[^[:graph:]]`.
   */
  val nonGraphChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonGraphCharMatch)))

  /**
   * A single printable character (visible character or space). Could be represented in a regular
   * expression as `[[:print:]]` or `\x20-\x7E`.
   */
  val printableChar: Regex[Char] = Coattr.pure(CharacterClasses.printableCharMatch)

  /**
   * Opposite of [[printableChar]]. Could be represented in a regular expression as `[^[:print:]]`.
   */
  val nonPrintableChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(NonEmptyList.one(CharacterClasses.nonPrintableCharMatch)))

  /**
   * A single punctuation character (`;`, `!`, etc).. Could be represented in a regular expression
   * as `[[:punct:]]`.
   */
  val punctuationChar: Regex[Char] = oneOfFR[NonEmptyList, Match[Char]](
    CharacterClasses.punctuationCharMatches
      .widen[Match[Char]]
      .map(Coattr.pure(_)))

  /**
   * Opposite of [[punctuationChar]]. Could be represented in a regular expression as
   * `[^[:punct:]]`.
   */
  val nonPunctuationChar: Regex[Char] =
    Coattr.pure(Match.NoneOf(CharacterClasses.nonPunctuationCharMatches))

  def matcher[F[_], A](
    r: Regex[A])(implicit orderingA: Ordering[A], foldableF: Foldable[F]): F[A] => Boolean = {
    implicit val orderA: Order[A] = Order.fromOrdering(orderingA)
    NFA.runNFA[F, Int, Match[A], A](Glushkov.kleeneToNFA(r), _.matches(_))
  }

  implicit private val indexedSeqFoldable: Foldable[IndexedSeq] =
    new IndexedSeqFoldable[IndexedSeq] {}

  def stringMatcher(r: Regex[Char]): String => Boolean = {
    val matcher = r.matcher[IndexedSeq]
    s => matcher(s)
  }
}
