package ceedubs.irrec
package regex

import cats.{Eval, Functor, Order}
import cats.data.State
import cats.implicits._
import qq.droste.{Algebra, AlgebraM}
import qq.droste.data.prelude._
import qq.droste.data.{CoattrF, Mu}
import qq.droste.scheme
import scala.collection.immutable.{SortedMap, SortedSet}

// a lot of the code in here is based on http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html
// TODO ceedubs document what these methods do
object Glushkov {

  type IndexedRegex[A] = Free[KleeneF, (Int, Match[A])]

  def kleeneLocalIsEmpty[A, I](k: KleeneF[LocalLanguage[A, I]]): Boolean = k match {
    case KleeneF.Times(l, r) => l.isEmpty && r.isEmpty
    case KleeneF.Plus(l, r) => l.isEmpty || r.isEmpty
    case KleeneF.Star(_) => true
    case KleeneF.Zero => false
    case KleeneF.One => true
  }

  def kleeneLocalLeading[A, I](k: KleeneF[LocalLanguage[A, I]]): List[(I, A)] = k match {
    case KleeneF.Times(l, r) => if (l.isEmpty) l.leading |+| r.leading else l.leading
    case KleeneF.Plus(l, r) => l.leading |+| r.leading
    case KleeneF.Star(x) => x.leading
    case KleeneF.One => List.empty
    case KleeneF.Zero => List.empty
  }

  def kleeneLocalTrailing[A, I](k: KleeneF[LocalLanguage[A, I]]): List[(I, A)] = k match {
    case KleeneF.Times(l, r) => if (r.isEmpty) l.trailing |+| r.trailing else r.trailing
    case KleeneF.Plus(l, r) => l.trailing |+| r.trailing
    case KleeneF.Star(x) => x.trailing
    case KleeneF.One => List.empty
    case KleeneF.Zero => List.empty
  }

  // TODO ceedubs formatting
  def kleeneLocalTransitions[A, I](k: KleeneF[LocalLanguage[A, I]])(implicit orderingI: Ordering[I]): SortedMap[I, List[(I, A)]] = {
    implicit val orderI: Order[I] = Order.fromOrdering(orderingI)
    k match {
      case KleeneF.Times(l, r) => l.transitions |+| r.transitions |+|
        l.trailing.foldMap{ case (ti, _) =>
          SortedMap((ti, r.leading.toList))
        }
      case KleeneF.Plus(l, r) => l.transitions |+| r.transitions
      case KleeneF.Star(x) => x.transitions |+|
        x.trailing.foldMap{ case (ti, _) =>
          SortedMap((ti, x.leading.toList))
        }
      case KleeneF.One => SortedMap.empty
      case KleeneF.Zero => SortedMap.empty
    }
  }

  def kleeneLocalLanguage[A, I](implicit orderingI: Ordering[I]): Algebra[KleeneF, LocalLanguage[A, I]] = Algebra{ ll =>
    LocalLanguage(
      isEmpty = kleeneLocalIsEmpty(ll),
      leading = kleeneLocalLeading(ll),
      trailing = kleeneLocalTrailing(ll),
      transitions = kleeneLocalTransitions(ll))
  }

  def indexLeaves[F[_]:Functor, A]: AlgebraM[State[Int, ?], CoattrF[F, A, ?], Free[F, (Int, A)]] =
    AlgebraM {
      CoattrF.un(_) match {
        case Left(a) => State((i: Int) => (i + 1, Mu(CoattrF.pure(i -> a))))
        case Right(z) => State.pure(Mu(CoattrF.roll(z)))
      }
    }

  def kleeneToLocalLanguage[A, I](implicit orderingI: Ordering[I]): Algebra[CoattrF[KleeneF, (I, A), ?], LocalLanguage[A, I]] = Algebra{
    CoattrF.un(_) match {
      case Left((i, ma)) => leafLocalLanguage(i, ma)
      case Right(kf) => kleeneLocalLanguage.apply(kf)
    }
  }

  def indexRegex[A]: Regex[A] => Eval[IndexedRegex[A]] = r =>
    scheme[Mu].cataM(indexLeaves[KleeneF, Match[A]]).apply(r).runA(1)

  // TODO ceedubs can we combine indexing leaves with another algebra and do a single pass?
  def regexToNFA[A](r: Regex[A]): NFA[Match[A], Int] = {
    val indexed = indexRegex(r).value
    val ll = scheme[Mu].cata(kleeneToLocalLanguage[Match[A], Int]).apply(indexed)
    localLanguageToNFA(ll)
  }

  def leafLocalLanguage[A, I](index: I, a: A)(implicit orderingI: Ordering[I]): LocalLanguage[A, I] = {
    val singletonList = List((index, a))
    LocalLanguage(
      isEmpty = false,
      leading = singletonList,
      trailing = singletonList,
      transitions = SortedMap.empty)
  }

  def localLanguageToNFA[A](ll: LocalLanguage[A, Int]): NFA[A, Int] = NFA(
    initStates = SortedSet(0),
    finalStates = (if (ll.isEmpty) SortedSet(0) else SortedSet.empty[Int]) |+| ll.trailing.map(_._1).to[SortedSet],
    transitions = ll.leading.foldMap{ l => SortedMap((0, List(l)))} |+| ll.transitions)

  // TODO ceedubs document
  final case class LocalLanguage[A, I](
    isEmpty: Boolean,
    leading: List[(I, A)],
    trailing: List[(I, A)],
    transitions: SortedMap[I, List[(I, A)]])
}
