package ceedubs.irrec
package regex

import cats.{~>, Applicative, Foldable}
import cats.free.FreeApplicative
import cats.data.{Chain, State}
import cats.implicits._

object CapturingKleeneA {
  def lift[M, In](k: Kleene[M]): CapturingKleeneA[M, In, Chain[In]] =
    FreeApplicative.lift(CaptureGroup(k, identity))

  private final case class CapturingKleeneBuilderState[A](
    ck: Option[CapturingKleene[Int, A]],
    captureIndex: Int)

  private object CapturingKleeneBuilderState {
    def empty[A]: CapturingKleeneBuilderState[A] = CapturingKleeneBuilderState(None, 1)
  }

  def statefulMatcher[F[_], M, In, S, Out](
    fa: CapturingKleeneA[M, In, Out],
    s0: S,
    doMatch: (S, M, In) => Option[S])(implicit foldableF: Foldable[F]): F[In] => Option[Out] = {
    // builder state
    type BS[A] = State[CapturingKleeneBuilderState[M], Map[Int, Chain[In]] => A]
    val captureGroupBuilder = λ[CaptureGroup[M, In, ?] ~> BS] { cg =>
      State { s =>
        val cgk = cg.kleene.captureAs(s.captureIndex)
        val cr2 = s.ck.fold(cgk)(_ * cgk)
        val s2 = CapturingKleeneBuilderState(Some(cr2), s.captureIndex + 1)
        (s2, chains => cg.mapCaptured(chains.getOrElse(s.captureIndex, Chain.empty)))
      }
    }

    val F = Applicative[State[CapturingKleeneBuilderState[M], ?]] compose Applicative[
      Map[Int, Chain[In]] => ?]
    val (builderState, groupsToOut) =
      fa.foldMap[BS](captureGroupBuilder)(F).run(CapturingKleeneBuilderState.empty).value
    builderState.ck.fold[F[In] => Option[Out]](_ => Some(groupsToOut(Map.empty))) { ck =>
      val nfa = Glushkov.capturingKleeneToNFA(ck)
      val captureMap = nfa.capturePath[F, (S, Map[Int, Chain[In]]), In]((s0, Map.empty)) {
        (s, a, b) =>
          doMatch(s._1, a._2, b).map { s2 =>
            (s2, s._2 |+| Map((a._1, Chain.one(b))))
          }
      }
      fa => captureMap(fa).map(s => groupsToOut(s._2))
    }
  }

  // TODO should we have a version that allows state to be carried along?
  def matcher[F[_], M, In, Out](fa: CapturingKleeneA[M, In, Out], doMatch: (M, In) => Boolean)(
    implicit foldableF: Foldable[F]): F[In] => Option[Out] =
    statefulMatcher[F, M, In, Unit, Out](fa, (), (_, m, i) => if (doMatch(m, i)) Some(()) else None)
}
