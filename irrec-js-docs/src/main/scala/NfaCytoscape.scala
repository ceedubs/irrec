package ceedubs.irrec
package docs

import ceedubs.irrec.regex._
import RegexPrettyPrinter.showCharMatch

import scala.scalajs.js
import org.scalajs.dom.raw.Element
import cats.Foldable
import cats.data.Writer
import cats.implicits._

object NfaCytoscape {
  implicit private val indexedSeqFoldable: Foldable[IndexedSeq] =
    new IndexedSeqFoldable[IndexedSeq] {}

  def nfaGraphDict(
    nfa: NFA[Int, Match[Char]],
    container: Element,
    candidate: String): js.Dictionary[js.Any] = {
    val (nodes, transitions) = nfa.transitions.toList.foldMap {
      case (node1, transitions) =>
        transitions.foldMap {
          case (node2, m) =>
            (Set(node2), List((node1, node2, showCharMatch(m))))
        } |+| ((Set(node1), List.empty))
    } |+| ((nfa.initStates ++ nfa.finalStates, List.empty))

    val nodeDicts = nodes.toList.map { node =>
      js.Dictionary[js.Any](
        "data" -> js.Dictionary[js.Any](
          "id" -> node,
          "initState" -> nfa.initStates.contains(node),
          "finalState" -> nfa.finalStates.contains(node)))
    }

    val successEdges = successfulEdges(nfa, candidate)

    val edgeDicts = transitions.map {
      case (node1, node2, m) =>
        js.Dictionary[js.Any](
          "data" ->
            js.Dictionary[js.Any](
              "id" -> s"$node1->$node2",
              "source" -> node1,
              "target" -> node2,
              "matched" -> successEdges.contains((node1, node2)),
              "label" -> m))
    }

    js.Dictionary(
      "container" -> container,
      "userZoomingEnabled" -> false,
      "layout" -> js.Dictionary[js.Any](
        "name" -> "breadthfirst",
        "directed" -> true,
        "grid" -> false,
        "maximal" -> true,
        "roots" -> js.Array(nfa.initStates.toList: _*),
        "animate" -> false),
      "style" -> js.Array(
        js.Dictionary(
          "selector" -> "edge",
          "style" -> js.Dictionary[js.Any](
            "curve-style" -> "bezier",
            "width" -> "1px",
            "target-arrow-shape" -> "triangle",
            "label" -> "data(label)",
            "text-background-shape" -> "roundrectangle",
            "text-background-padding" -> "3px",
            "text-background-color" -> "#17B890",
            "text-background-opacity" -> 1
          )
        ),
        js.Dictionary(
          "selector" -> "edge[?matched]",
          "style" -> js
            .Dictionary[js.Any]("line-color" -> "#17B890", "target-arrow-color" -> "#17B890")),
        js.Dictionary(
          "selector" -> "edge[!matched]",
          "style" -> js
            .Dictionary[js.Any]("line-style" -> "dashed")),
        js.Dictionary(
          "selector" -> "node[!initState][!finalState]",
          "style" -> js
            .Dictionary[js.Any]("shape" -> "ellipse", "height" -> "1px", "width" -> "1px")),
        js.Dictionary(
          "selector" -> "node[?finalState]",
          "style" -> js
            .Dictionary[js.Any]("shape" -> "star", "height" -> "14px", "width" -> "14px")),
        js.Dictionary(
          "selector" -> "node[?initState]",
          "style" -> js.Dictionary[js.Any]("shape" -> "vee", "width" -> "14px", "height" -> "14px"))
      ),
      "elements" -> js
        .Dictionary("nodes" -> js.Array(nodeDicts: _*), "edges" -> js.Array(edgeDicts: _*))
    )
  }

  def successfulEdges(nfa: NFA[Int, Match[Char]], candidate: String): Set[(Int, Int)] =
    NFA
      .runNFAWithEffect[IndexedSeq, Writer[Set[(Int, Int)], ?], Int, Match[Char], Char](
        nfa,
        matches = { (n1, n2, m, c) =>
          val isMatch = m.matches(c)
          val log = if (isMatch) Set((n1, n2)) else Set.empty[(Int, Int)]
          Writer(log, isMatch)
        }
      )
      .apply(candidate)
      .written
}
