package exastencils.base.l2

import scala.collection.mutable.Stack

import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger.Logger

class L2_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  override def enter(node : Node) : Unit = {
    node match {
      // TODO
      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      // TODO
      case _ =>
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
