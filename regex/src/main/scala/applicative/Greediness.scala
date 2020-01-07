package ceedubs.irrec
package regex
package applicative

sealed abstract class Greediness extends Product with Serializable

object Greediness {
  case object Greedy extends Greediness
  case object NonGreedy extends Greediness
}
