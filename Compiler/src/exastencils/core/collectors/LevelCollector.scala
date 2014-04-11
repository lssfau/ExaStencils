package exastencils.core.collectors

import exastencils.datastructures._
import exastencils.datastructures.l4._

class LevelCollector extends Collector {
  var curLevel = 0

  def enter(node : Node) : Unit = {
    node match {
      case FunctionStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _, _, _) => curLevel = level
      case _ =>
    }
  }

  def leave(node : Node) : Unit = {

  }

  def reset() : Unit = {
    curLevel = 0
  }
}
