package ceedubs.irrec
package regex

import cats.Order
import cats.implicits._
import higherkindness.droste.Algebra
import higherkindness.droste.data.prelude._
import higherkindness.droste.data.CoattrF
import higherkindness.droste.scheme
import scala.collection.immutable.SortedMap

/**
 * Functions for converting a regular expression into an NFA.
 *
 * Much of the code in here is based on the blog post [[http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html "Extending Glushkov NFA with sub matching over Strings"]]
 * by Kenny Zhuo Ming Lu.
 */
object Glushkov {
  def kleeneLocalIsEmpty[I, A](k: KleeneF[LocalLanguage[I, A]]): Boolean = k match {
    case KleeneF.Times(l, r) => l.isEmpty && r.isEmpty
    case KleeneF.Plus(l, r) => l.isEmpty || r.isEmpty
    case KleeneF.Star(_) => true
    case KleeneF.Zero => false
    case KleeneF.One => true
  }

  def kleeneLocalLeading[I, A](k: KleeneF[LocalLanguage[I, A]]): List[(I, A)] = k match {
    case KleeneF.Times(l, r) => if (l.isEmpty) l.leading |+| r.leading else l.leading
    case KleeneF.Plus(l, r) => l.leading |+| r.leading
    case KleeneF.Star(x) => x.leading
    case KleeneF.One => List.empty
    case KleeneF.Zero => List.empty
  }

  def kleeneLocalTrailing[I, A](k: KleeneF[LocalLanguage[I, A]]): List[(I, A)] = k match {
    case KleeneF.Times(l, r) => if (r.isEmpty) l.trailing |+| r.trailing else r.trailing
    case KleeneF.Plus(l, r) => l.trailing |+| r.trailing
    case KleeneF.Star(x) => x.trailing
    case KleeneF.One => List.empty
    case KleeneF.Zero => List.empty
  }

  def kleeneLocalTransitions[I, A](k: KleeneF[LocalLanguage[I, A]])(
    implicit orderingI: Ordering[I]): SortedMap[I, List[(I, A)]] = {
    implicit val orderI: Order[I] = Order.fromOrdering(orderingI)
    k match {
      case KleeneF.Times(l, r) =>
        l.transitions |+| r.transitions |+|
          l.trailing.foldMap {
            case (ti, _) =>
              SortedMap((ti, r.leading.toList))
          }
      case KleeneF.Plus(l, r) => l.transitions |+| r.transitions
      case KleeneF.Star(x) =>
        x.transitions |+|
          x.trailing.foldMap {
            case (ti, _) =>
              SortedMap((ti, x.leading.toList))
          }
      case KleeneF.One => SortedMap.empty
      case KleeneF.Zero => SortedMap.empty
    }
  }

  def kleeneLocalLanguage[I, A](
    implicit orderingI: Ordering[I]): Algebra[KleeneF, LocalLanguage[I, A]] = Algebra { ll =>
    LocalLanguage(
      isEmpty = kleeneLocalIsEmpty(ll),
      leading = kleeneLocalLeading(ll),
      trailing = kleeneLocalTrailing(ll),
      transitions = kleeneLocalTransitions(ll))
  }

  def indexedKleeneToLocalLanguage[I, A](
    implicit orderingI: Ordering[I]): Algebra[CoattrF[KleeneF, (I, A), ?], LocalLanguage[I, A]] =
    Algebra {
      CoattrF.un(_) match {
        case Left((i, ma)) => LocalLanguage.leaf(i, ma)
        case Right(kf) => kleeneLocalLanguage.apply(kf)
      }
    }

  // TODO ceedubs can we combine indexing leaves with another algebra and do a single pass?
  def kleeneToNFA[A](k: Kleene[A]): NFA[Int, A] = {
    val indexed = scheme.cataM(CoattrSchemes.indexLeaves[KleeneF, A]).apply(k).runA(1).value
    val ll = scheme.cata(indexedKleeneToLocalLanguage[Int, A]).apply(indexed)
    LocalLanguage.intLocalLanguageToNFA(ll)
  }
}
