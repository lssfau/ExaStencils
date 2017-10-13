package exastencils.util.l4

import scala.collection.mutable.Stack

import exastencils.base.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.logger.Logger

/// L4_LevelCollector

class L4_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L4_LevelSpecification]) = {
    level match {
      case Some(L4_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def enterLevel(level : Int) = levelStack.push(level)

  def leaveLevel(level : Option[L4_LevelSpecification]) = {
    level match {
      case Some(L4_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }
  def leaveLevel(level : Int) = levelStack.pop()

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L4_LeveledKnowledgeDecl => enterLevel(decl.levels)
      case fct : L4_FunctionDecl          => enterLevel(fct.levels)
      case fct : L4_LeveledFunction       => enterLevel(fct.level)
      case _                              =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L4_LeveledKnowledgeDecl => leaveLevel(decl.levels)
      case fct : L4_FunctionDecl          => leaveLevel(fct.levels)
      case fct : L4_LeveledFunction       => leaveLevel(fct.level)
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
