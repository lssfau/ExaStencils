package exastencils.core.collectors

import exastencils.base.ir.IR_Statement
import exastencils.datastructures._

class StatementCollector extends Collector {
  var insideStatement : Int = 0

  override def enter(node : Node) : Unit = {
    node match {
      case s : IR_Statement => insideStatement += 1
      case _                =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case s : IR_Statement => insideStatement -= 1
      case _                =>
    }
  }

  override def reset() : Unit = {
    insideStatement = 0
  }
}
