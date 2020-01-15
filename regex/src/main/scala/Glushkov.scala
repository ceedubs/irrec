package ceedubs.irrec
package regex

import cats.Order
import cats.data.State
import cats.implicits._
import higherkindness.droste.{Algebra, AlgebraM}
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

  //def indexedLabels[A](
  //  x: SemirngF[LabeledKleene[Boolean, A]]): State[Int, SemirngF[LabeledKleene[Option[Int], A]]] =
  //  x match {
  //    case Plus(l, r) => (indexedLabel(l), indexedLabel(r)).mapN(Plus.apply)
  //    case Times(l, r) => (indexedLabel(l), indexedLabel(r)).mapN(Times.apply)
  //  }

  //def indexLabeledKleeneSemirng[L, A](
  //  x: SemirngF[LabeledKleene[L, A]]): State[Int, SemirngF[LabeledKleene[L, (Int, A)]]] =
  //  x match {
  //    case SemirngF.Plus(l, r) =>
  //      (indexKleeneLeaves(l.value), indexKleeneLeaves(r.value)).mapN((k1, k2) =>
  //        SemirngF.Plus(l.copy(value = k1), r.copy(value = k2)))
  //    case SemirngF.Times(l, r) =>
  //      (indexKleeneLeaves(l.value), indexKleeneLeaves(r.value)).mapN((k1, k2) =>
  //        SemirngF.Times(l.copy(value = k1), r.copy(value = k2)))
  //  }

  def indexKleeneLeaves[A](k: Kleene[A]): State[Int, Kleene[(Int, A)]] =
    scheme.cataM(CoattrSchemes.indexLeaves[KleeneF, A]).apply(k)

  // TODO ceedubs should this be here?
  final case class CapturingRegexLabelState(leafIndex: Int, captureIndex: Int)

  object CapturingRegexLabelState {
    // TODO ceedubs should these both be 1-indexed? Document.
    val init: CapturingRegexLabelState = CapturingRegexLabelState(leafIndex = 1, captureIndex = 1)

    def zoomLeafIndex[A]: State[Int, A] => State[CapturingRegexLabelState, A] =
      LensState.zoomState[Int, CapturingRegexLabelState, A](
        _.leafIndex,
        (i, s) => s.copy(leafIndex = i))

    def zoomCaptureIndex[A]: State[Int, A] => State[CapturingRegexLabelState, A] =
      LensState.zoomState[Int, CapturingRegexLabelState, A](
        _.captureIndex,
        (i, s) => s.copy(captureIndex = i))
  }

  // TODO ceedubs
  def bolth2[A](lk: LabeledKleene[Boolean, A])
    : State[CapturingRegexLabelState, LabeledKleene[Option[Int], (Int, A)]] =
    for {
      withLabelIndices <- CapturingRegexLabelState.zoomLeafIndex(indexedLabel(lk))
      withLeafIndices <- CapturingRegexLabelState.zoomCaptureIndex(
        indexKleeneLeaves(withLabelIndices.value))
    } yield withLabelIndices.copy(value = withLeafIndices)

  // TODO document (the Int in the state is the leaf index)
  def labeledKleeneToLocalLanguageAlgebra[L, A]: AlgebraM[
    State[Int, ?],
    CoattrF[SemirngF, LabeledKleene[L, A], ?],
    LocalLanguage[Int, (L, A)]] =
    AlgebraM {
      CoattrF.un(_) match {
        case Left(lk) =>
          indexKleeneLeaves(lk.value).map { indexed =>
            scheme
              .cata(indexedKleeneToLocalLanguage[Int, A])
              .apply(indexed)
              .map(a => (lk.label, a))
          }
        case Right(x) => State.pure(kleeneLocalLanguage[Int, (L, A)].apply(x.toKleeneF))
      }
    }

  // TODO ceedubs
  def bolthAlgebra[A]: AlgebraM[
    State[CapturingRegexLabelState, ?],
    CoattrF[SemirngF, LabeledKleene[Boolean, A], ?],
    LocalLanguage[Int, (Option[Int], A)]] =
    AlgebraM {
      CoattrF.un(_) match {
        // TODO ceedubs clean up
        case Left(l) =>
          bolth2(l).map { x =>
            val temp = scheme.cata(indexedKleeneToLocalLanguage[Int, A]).apply(x.value)
            temp.map(a => (x.label, a))
          }
        case Right(x) => State.pure(kleeneLocalLanguage[Int, (Option[Int], A)].apply(x.toKleeneF))
      }
    }

  // TODO ceedubs can we combine indexing leaves with another algebra and do a single pass?
  def kleeneToNFA[A](k: Kleene[A]): NFA[Int, A] = {
    val indexed = indexKleeneLeaves(k).runA(1).value
    val ll = scheme.cata(indexedKleeneToLocalLanguage[Int, A]).apply(indexed)
    LocalLanguage.intLocalLanguageToNFA(ll)
  }

  // TODO ceedubs naming
  def tempToNFA[A](k: CapturingKleene[Boolean, A]): NFA[Int, (Option[Int], A)] = {
    val x = scheme.cataM(bolthAlgebra[A]).apply(k).runA(CapturingRegexLabelState.init).value
    LocalLanguage.intLocalLanguageToNFA(x)
  }

  def capturingKleeneToNFA[L, A](k: CapturingKleene[L, A]): NFA[Int, (L, A)] = {
    val x = scheme
      .cataM(labeledKleeneToLocalLanguageAlgebra[L, A])
      .apply(k)
      .runA(CapturingRegexLabelState.init.leafIndex)
      .value
    LocalLanguage.intLocalLanguageToNFA(x)
  }
}
