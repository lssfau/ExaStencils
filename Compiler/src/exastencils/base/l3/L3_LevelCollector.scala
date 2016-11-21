package exastencils.base.l3

import scala.collection.mutable.Stack

import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger.Logger

class L3_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  override def enter(node : Node) : Unit = {
    node match {
      case fct @ L3_Function(_, Some(level), _, _, _) => levelStack.push(level.resolveLevel)
      case _                                          =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case fct @ L3_Function(_, Some(level), _, _, _) => levelStack.pop
      case _                                          =>
    }
  }

  override def reset() : Unit = {
    levelStack.clear
  }

  def inLevelScope : Boolean = levelStack.nonEmpty

  def getCurrentLevel : Int = {
    if (levelStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      -1
    } else {
      levelStack.head
    }
  }
}
