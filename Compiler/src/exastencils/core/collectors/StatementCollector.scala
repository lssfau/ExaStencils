package exastencils.core.collectors

import exastencils.datastructures._
import exastencils.datastructures.ir._

class StatementCollector extends Collector {
  var insideStatement : Int = 0

  override def enter(node : Node) : Unit = {
    node match {
      case s : Statement => insideStatement += 1
      case _             =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case s : Statement => insideStatement -= 1
      case _             =>
    }
  }

  override def reset() : Unit = {
    insideStatement = 0
  }
}
