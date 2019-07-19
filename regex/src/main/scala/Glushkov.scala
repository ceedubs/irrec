package ceedubs.irrec
package regex

import cats.Order
import cats.data.State
import cats.implicits._
import higherkindness.droste.Algebra
import higherkindness.droste.data.prelude._
import higherkindness.droste.data.CoattrF
import higherkindness.droste.scheme
import scala.collection.immutable.SortedMap
import ceedubs.irrec.regex.SemirngF.Plus
import ceedubs.irrec.regex.SemirngF.Times

/**
 * Functions for converting a regular expression into an NFA.
 *
 * Much of the code in here is based on the blog post [[http://luzhuomi.blogspot.com/2012/06/extending-glushkov-nfa-with-sub.html "Extending Glushkov NFA with sub matching over Strings"]]
 * by Kenny Zhuo Ming Lu.
 */
object Glushkov {

  // TODO ceedubs consider just always having a label on LocalLanguage and making it Unit when we don't care about it
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

  // TODO ceedubs document...this has a fairly specific use-case
  def indexedLabel[A](lk: LabeledKleene[Boolean, A]): State[Int, LabeledKleene[Option[Int], A]] =
    if (lk.label) State(i => (i + 1, lk.copy(label = Some(i))))
    else State.pure(lk.copy(label = None))

  def indexedLabels[A](
    x: SemirngF[LabeledKleene[Boolean, A]]): State[Int, SemirngF[LabeledKleene[Option[Int], A]]] =
    x match {
      case Plus(l, r) => (indexedLabel(l), indexedLabel(r)).mapN(Plus.apply)
      case Times(l, r) => (indexedLabel(l), indexedLabel(r)).mapN(Times.apply)
    }

  def indexLabeledKleeneSemirng[L, A](
    x: SemirngF[LabeledKleene[L, A]]): State[Int, SemirngF[LabeledKleene[L, (Int, A)]]] =
    x match {
      case SemirngF.Plus(l, r) =>
        (indexKleeneLeaves(l.value), indexKleeneLeaves(r.value)).mapN((k1, k2) =>
          SemirngF.Plus(l.copy(value = k1), r.copy(value = k2)))
      case SemirngF.Times(l, r) =>
        (indexKleeneLeaves(l.value), indexKleeneLeaves(r.value)).mapN((k1, k2) =>
          SemirngF.Times(l.copy(value = k1), r.copy(value = k2)))
    }

  def indexKleeneLeaves[A](k: Kleene[A]): State[Int, Kleene[(Int, A)]] =
    scheme.cataM(CoattrSchemes.indexLeaves[KleeneF, A]).apply(k)

  // TODO ceedubs should this be here?
  final case class CapturingRegexLabelState(leafIndex: Int, captureIndex: Int)

  object CapturingRegexLabelState {
    def zoomState[S1, S2, A](state: State[S1, A])(set: S1 => S2, get: S2 => S1): State[S2, A] =
      state.dimap(get)(set)

    def zoomLeafIndex[A](state: State[Int, A]): State[CapturingRegexLabelState, A] = {
      // TODO ceedubs
      import cats.data.IndexedStateT
      IndexedStateT.apply{ s =>
        
      }
      state.dimap(_.leafIndex)(i => labels.copy(leafIndex = i))
    }
  }

  // TODO ceedubs naming
  def bolth[A](x: SemirngF[LabeledKleene[Boolean, A]]): State[CapturingRegexLabelState, SemirngF[LabeledKleene[Option[Int], (Int, A)]]] =
    for {
      labels <- State.get[CapturingRegexLabelState]
      // this would be much cleaner with lenses...
      withLabels <- indexedLabels(x).dimap((_: CapturingRegexLabelState).captureIndex)(i => labels.copy(captureIndex = i))
      withIndices <- indexLabeledKleeneSemirng(withLabels).dimap((_: CapturingRegexLabelState).leafIndex)(i => labels.copy(leafIndex = i))
    } yield withIndices

  // TODO ceedubs
  import higherkindness.droste.AlgebraM
  import higherkindness.droste.data.Coattr
  def bolthAlgebra[A]: AlgebraM[State[CapturingRegexLabelState, ?], CoattrF[SemirngF, LabeledKleene[Boolean, A], ?], Coattr[SemirngF, LabeledKleene[Option[Int], (Int, A)]]] =
    AlgebraM[State[CapturingRegexLabelState, ?], CoattrF[SemirngF, LabeledKleene[Boolean, A], ?], Coattr[SemirngF, LabeledKleene[Option[Int], (Int, A)]]] {
      CoattrF.un(_) match {
        // TODO ceedubs this is repeated in a couple of places
        case Left(l) => indexedLabel(l).dimap((_: CapturingRegexLabelState).leafIndex)(i => labels.copy(leafIndex = i)).map(Coattr.pure(_))
        case Right(_) => 3
      }
    }

  // TODO ceedubs naming
  //def todo[A](k: CapturingKleene[Boolean, A]): NFA[Int, (Int, A)] = {
  //  val x = AlgebraM(bolth(_))
  //  //val indexed = scheme.cataM()
  //  x
  //}

  // TODO ceedubs can we combine indexing leaves with another algebra and do a single pass?
  def kleeneToNFA[A](k: Kleene[A]): NFA[Int, A] = {
    val indexed = indexKleeneLeaves(k).runA(1).value
    val ll = scheme.cata(indexedKleeneToLocalLanguage[Int, A]).apply(indexed)
    LocalLanguage.intLocalLanguageToNFA(ll)
  }
}
