package exastencils.datastructures

import scala.collection.GenTraversableOnce

/** The basic trait every matchable (in terms of [[exastencils.datastructures.Transformation]] matching) entity has to subclass. */
trait Node extends Annotatable with Product with Serializable {
  var location = SourceLocation()
}

final case class HelperNode(node : Node) extends Node

/** A type for returning more than a single [[exastencils.datastructures.Node]] per [[exastencils.datastructures.Transformation]]. */
final class NodeList(var nodes : GenTraversableOnce[Node])

package object ir {/* FIXME: this is only a placeholder until something reasonable is in place */ type StatementList = NodeList }
