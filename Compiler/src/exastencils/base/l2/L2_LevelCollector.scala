package exastencils.base.l2

import scala.collection.mutable.Stack

import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l2._
import exastencils.logger.Logger

/// L2_LevelCollector

class L2_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L2_LevelSpecification]) = {
    level match {
      case Some(L2_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def leaveLevel(level : Option[L2_LevelSpecification]) = {
    level match {
      case Some(L2_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L2_LeveledKnowledgeDecl => enterLevel(decl.levels)
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L2_LeveledKnowledgeDecl => leaveLevel(decl.levels)
      case _                              =>
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
