package exastencils.util.ir

import scala.collection.mutable.Stack

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.logger.Logger

/// IR_LevelCollector

class IR_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Int) = levelStack.push(level)
  def leaveLevel(level : Int) = levelStack.pop()

  override def enter(node : Node) : Unit = {
    node match {
      case fct : IR_LeveledFunction              => enterLevel(fct.level)
      case loop : IR_LoopOverPoints              => enterLevel(loop.field.level)
      case loop : IR_LoopOverPointsInOneFragment => enterLevel(loop.field.level)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case fct : IR_LeveledFunction              => leaveLevel(fct.level)
      case loop : IR_LoopOverPoints              => leaveLevel(loop.field.level)
      case loop : IR_LoopOverPointsInOneFragment => leaveLevel(loop.field.level)
      case _                                     =>
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
