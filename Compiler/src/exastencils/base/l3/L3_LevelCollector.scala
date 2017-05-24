package exastencils.base.l3

import scala.collection.mutable.Stack

import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l3.L3_LeveledKnowledgeDecl
import exastencils.logger.Logger

/// L3_LevelCollector

class L3_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L3_LevelSpecification]) = {
    level match {
      case Some(L3_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def leaveLevel(level : Option[L3_LevelSpecification]) = {
    level match {
      case Some(L3_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L3_LeveledKnowledgeDecl => enterLevel(decl.levels)
      case fct : L3_Function              => enterLevel(fct.levels)
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L3_LeveledKnowledgeDecl => leaveLevel(decl.levels)
      case fct : L3_Function              => leaveLevel(fct.levels)
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
