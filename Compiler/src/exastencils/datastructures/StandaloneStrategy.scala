package exastencils.datastructures

import scala.collection.mutable

/// StandaloneStrategy

// FIXME Integrate this into the framework. Ideally this should represent a simple read-only transformation.
// No need for all the transaction machinery.
/** Only implements applyStandalone(node: Node) : Unit. */
abstract class StandaloneStrategy extends QuietDefaultStrategy("Some StandaloneStrategy") {
  override final def apply(applyAtNode : Option[Node] = None) : Unit = ???
  override final def applyStandalone[T](nodes : mutable.Buffer[T]) : Unit = ???
  override final def applyStandalone[T](nodes : Seq[T]) : Seq[T] = ???
}
