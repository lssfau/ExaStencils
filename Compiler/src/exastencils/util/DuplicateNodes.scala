package exastencils.util

import exastencils.core.Duplicate
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.datastructures.ir._

class DuplicateNodes extends DefaultStrategy("Eliminate multiple usage of node instances") {
  private final val instances = new java.util.IdentityHashMap[Node, Any]()

  this += new Transformation("duplicate", new PartialFunction[Node, Transformation.OutputType] {

    override def isDefinedAt(node : Node) : Boolean = {
      return Duplicate.clonable(node) && instances.put(node, this) != null
    }

    override def apply(node : Node) : Transformation.OutputType = {
      // instances.put(dup, this) // we just created a new instance, so it is impossible we can find it anywhere else in the AST
      return Duplicate(node)
    }
  })
}
