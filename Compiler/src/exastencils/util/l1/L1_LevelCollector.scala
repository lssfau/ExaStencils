package exastencils.util.l1

import scala.collection.mutable.Stack

import exastencils.base.l1._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l1._
import exastencils.logger.Logger

/// L1_LevelCollector

class L1_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L1_LevelSpecification]) = {
    level match {
      case Some(L1_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def leaveLevel(level : Option[L1_LevelSpecification]) = {
    level match {
      case Some(L1_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L1_LeveledKnowledgeDecl => enterLevel(decl.levels)
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L1_LeveledKnowledgeDecl => leaveLevel(decl.levels)
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
