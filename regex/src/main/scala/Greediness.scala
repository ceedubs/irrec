package ceedubs.irrec
package regex

sealed abstract class Greediness extends Product with Serializable

object Greediness {
  case object Greedy extends Greediness
  case object NonGreedy extends Greediness
}
