package ceedubs.irrec.regex

import ceedubs.irrec.regex.Greediness._

sealed abstract class Quantifier extends Product with Serializable {
  import Quantifier._

  def pprint: String =
    this match {
      case Exact(n) => s"{$n}"
      case Range(min, max, g) => s"{$min,${max.getOrElse("")}}${pprintGreediness(g)}"
      case Optional(g) => s"?${pprintGreediness(g)}"
    }
}

object Quantifier {
  final case class Exact(n: Int) extends Quantifier
  final case class Range(lowerInclusive: Int, upperInclusive: Option[Int], greediness: Greediness)
      extends Quantifier
  final case class Optional(greediness: Greediness) extends Quantifier

  def pprintGreediness(greediness: Greediness): String =
    greediness match {
      case Greedy => ""
      case NonGreedy => "?"
    }
}
