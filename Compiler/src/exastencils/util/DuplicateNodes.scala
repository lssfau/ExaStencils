package exastencils.util

import exastencils.core.Duplicate
import exastencils.core.collectors.StackCollector
import exastencils.datastructures._
import exastencils.logger.Logger

object DuplicateNodes extends DefaultStrategy("Eliminate multiple usage of node instances") {
  // note: instances must be cleared between runs
  var instances = new java.util.IdentityHashMap[Node, Any]()

  var collector = new StackCollector()
  this.register(collector)

  var printWarnings = true
  var printStack = false

  this += new Transformation("Duplicate", new PartialFunction[Node, Transformation.OutputType] {
    override def isDefinedAt(node : Node) : Boolean = {
      Duplicate.clonable(node) && instances.put(node, this) != null
    }

    override def apply(node : Node) : Transformation.OutputType = {
      // instances.put(dup, this) // we just created a new instance, so it is impossible we can find it anywhere else in the AST
      if (printWarnings) {
        Logger.warn("Eliminated double reference by cloning: " + node)
        if (printStack) {
          Logger.warn("Parents are:" + collector.stack.elems.map(n => n.getClass.getName).mkString(" => "))
          collector.stack.elems.foreach(Logger.warn(_))
        }
      }
      Duplicate(node)
    }
  }
  )
}
