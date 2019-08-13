package ceedubs.irrec
package regex

import cats.data.{IndexedStateT, State}

// TODO ceedubs document
object LensState {
  def zoomState[S1, S2, A](get: S2 => S1, set: (S1, S2) => S2)(state: State[S1, A]): State[S2, A] =
    IndexedStateT { s2 =>
      state.run(get(s2)).map { case (s1, a) =>
        (set(s1, s2), a)
      }
    }
}
