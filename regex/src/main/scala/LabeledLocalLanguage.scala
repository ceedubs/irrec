package ceedubs.irrec
package regex

import scala.collection.immutable.SortedMap

// TODO ceedubs document
// TODO ceedubs should this exist or should we just reuse LocalLanguage and attach a unit label?
final case class LabeledLocalLanguage[I, L, A](
  isEmpty: Boolean,
  leading: List[(I, A)],
  trailing: List[(I, A)],
  transitions: SortedMap[I, List[(I, L, A)]])

// we need to index the leaves in all of the leaf regular expressions.
// Separately, we need to index each Left in the CapturingRegex, to give them a capture group number.
