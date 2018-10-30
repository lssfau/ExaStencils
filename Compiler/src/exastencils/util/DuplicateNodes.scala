package exastencils.util

import exastencils.base.ir.IR_Node
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.util.ir.IR_StackCollector

object DuplicateNodes extends DefaultStrategy("Eliminate multiple usage of node instances") {
  // note: instances must be cleared between runs
  var instances = new java.util.IdentityHashMap[Node, List[IR_Node]]()

  var collector = new IR_StackCollector()
  this.register(collector)
  this.onBefore = () => {
    instances.clear()
    this.resetCollectors()
  }

  var printWarnings = true
  var printStack = false

  this += new Transformation("Duplicate", new PartialFunction[Node, Transformation.OutputType] {
    override def isDefinedAt(node : Node) : Boolean = {
      if (instances.containsKey(node))
        return true
      if (Duplicate.clonable(node))
        instances.put(node, collector.stack)
      return false
    }

    override def apply(node : Node) : Transformation.OutputType = {
      if (printWarnings) {
        Logger.warn("Eliminated double reference by cloning: " + node)
        if (printStack) {
          val location1 = collector.stack
          val location2 = instances.get(node)
          Logger.warn("  location 1 parents are: " + location1.view.map(n => n.getClass.getName).mkString(" => "))
          Logger.warn("  location 2 parents are: " + location2.view.map(n => n.getClass.getName).mkString(" => "))
        }
      }
      // instances.put(dup, this) // we just created a new instance, so it is impossible we can find it anywhere else in the AST
      Duplicate(node)
    }
  }
  )
}
