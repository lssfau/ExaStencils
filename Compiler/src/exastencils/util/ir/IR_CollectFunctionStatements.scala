package exastencils.util.ir

import scala.collection.mutable.HashSet

import exastencils.base.ir.IR_Function
import exastencils.core.ObjectWithState
import exastencils.datastructures._

/// IR_CollectFunctionStatements

object IR_CollectFunctionStatements extends DefaultStrategy("Collecting internal function statements") with ObjectWithState {
  var internalFunctions = HashSet[String]()

  override def clear() = internalFunctions.clear()

  override def apply(node : Option[Node] = None) : Unit = {
    clear()
    super.apply(node)
  }

  this += new Transformation("Collecting", {
    case fct : IR_Function =>
      internalFunctions += fct.name
      fct
  }, false)
}
