package exastencils.core.collectors

import scala.collection.mutable.Stack

import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.logger._

class IRLevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  override def enter(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverPoints              => levelStack.push(loop.field.level)
      case loop : IR_LoopOverPointsInOneFragment => levelStack.push(loop.field.level)
      case _                                     =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case loop : IR_LoopOverPoints              => levelStack.pop
      case loop : IR_LoopOverPointsInOneFragment => levelStack.pop
      case _                                     =>
    }
  }

  override def reset() : Unit = {
    levelStack.clear
  }

  def inLevelScope : Boolean = !levelStack.isEmpty

  def getCurrentLevel : Int = {
    if (levelStack.isEmpty) {
      Logger.dbg("Trying to access level outside of a valid scope")
      -1
    } else {
      levelStack.head
    }
  }
}
