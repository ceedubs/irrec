package ceedubs.irrec
package regex

import cats.implicits._
import cats.{Foldable, Monad}
import cats.data.EitherT
import scala.collection.immutable.{SortedMap, SortedSet}

// TODO think about which methods we want exposed on this class
final case class NFA[I, A](
  initStates: SortedSet[I],
  finalStates: SortedSet[I],
  transitions: SortedMap[I, List[(I, A)]]) {

  // TODO make partially-applied helper?
  def capturePath[F[_], S, B](s0: S)(doMatch: (S, A, B) => Option[S])(implicit foldableF: Foldable[F]): F[B] => Option[S] =
    NFA.captureNFAPath(this, s0)(doMatch)
}

object NFA {
  /**
   * Similar to [[runNFA]], but optimized for a short input `F[A]`.
   *
   * This version will always consume the entire `F[A]`, even if it reaches a point in which a match
   * is impossible. However, it makes fewer allocations and has less overhead for each element that
   * it processes, so if the input `F[A]` is short, then it can be faster than [[runNFA]].
   */
  def runNFAShortInput[F[_], I, B, A](nfa: NFA[I, B], matches: (B, A) => Boolean)(
    implicit foldableF: Foldable[F]): F[A] => Boolean = { (fa: F[A]) =>
    import nfa.initStates.ordering
    val finalStates: SortedSet[I] = fa.foldLeft(nfa.initStates)(
      (currentStates, a) =>
        currentStates
          .flatMap(
            i =>
              nfa.transitions
                .getOrElse(i, List.empty)
                .collect { case (i, b) if matches(b, a) => i }))
    nfa.finalStates.exists(finalStates.contains(_))
  }

  // TODO would be more efficient to duplicate code
  def runNFA[F[_], I, B, A](nfa: NFA[I, B], matches: (B, A) => Boolean)(
    implicit foldableF: Foldable[F]): F[A] => Boolean = {
    val toOption: F[A] => Option[Unit] =
      captureNFAPath(nfa, ())((_, b, a) => if (matches(b, a)) Some(()) else None)
    fa => toOption(fa).isDefined
  }

  def captureNFAPath[F[_], S, I, B, A](nfa: NFA[I, B], s0: S)(doMatch: (S, B, A) => Option[S])(
    implicit foldableF: Foldable[F]): F[A] => Option[S] = {
    val init: List[(S, I)] = nfa.initStates.toList.map { node =>
      (s0, node)
    }

    { (fa: F[A]) =>
      val finalStates: Either[Unit, Iterator[(S, I)]] =
        fa.foldM[Either[Unit, ?], Iterator[(S, I)]](init.iterator) { (currentStates, a) =>
          val nextStates = currentStates.flatMap {
            case (s, i) =>
              nfa.transitions
                .getOrElse(i, List.empty)
                .flatMap {
                  case (i, b) =>
                    doMatch(s, b, a).map(s => (s, i))
                }
          }
          if (nextStates.isEmpty) Left(()) else Right(nextStates)
        }
      finalStates.fold(
        _ => None,
        states =>
          states.collectFirst {
            case s if nfa.finalStates.contains(s._2) => s._1
          })
    }
  }

  def runNFAWithEffect[F[_], G[_], I, B, A](nfa: NFA[I, B], matches: (I, I, B, A) => G[Boolean])(
    implicit foldableF: Foldable[F],
    G: Monad[G]): F[A] => G[Boolean] = { (fa: F[A]) =>
    val finalStates: EitherT[G, Unit, List[I]] = fa.foldM(nfa.initStates.toList) {
      (currentStates, a) =>
        val nextStates = currentStates
          .flatTraverse(
            i =>
              nfa.transitions
                .getOrElse(i, List.empty)
                .filterA { case (i2, b) => matches(i, i2, b, a) })
          .map(_.map(_._1))
        EitherT(nextStates.map(states => if (states.isEmpty) Left(()) else Right(states)))
    }
    finalStates.fold(_ => false, states => states.exists(nfa.finalStates.contains(_)))
  }
}
