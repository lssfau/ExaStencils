package exastencils.base.l4

import scala.collection.mutable.Stack

import exastencils.core.collectors.Collector
import exastencils.datastructures.Node
import exastencils.knowledge.l4.L4_LeveledKnowledgeDecl
import exastencils.logger.Logger

class L4_LevelCollector extends Collector {
  private val levelStack = new Stack[Int]

  def enterLevel(level : Option[L4_LevelSpecification]) = {
    level match {
      case Some(L4_SingleLevel(lvl)) => levelStack.push(lvl)
      case _                         =>
    }
  }
  def leaveLevel(level : Option[L4_LevelSpecification]) = {
    level match {
      case Some(L4_SingleLevel(_)) => levelStack.pop()
      case _                       =>
    }
  }

  override def enter(node : Node) : Unit = {
    node match {
      case decl : L4_LeveledKnowledgeDecl => enterLevel(decl.levels)
      //case fct : L4_Function              => enterLevel(fct.levels)
      case L4_Function(L4_LeveledIdentifier(_, level), _, _, _, _) => levelStack.push(level.resolveLevel)
      case _                                                       =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case decl : L4_LeveledKnowledgeDecl => leaveLevel(decl.levels)
      //case fct : L4_Function              => leaveLevel(fct.levels)
      case L4_Function(L4_LeveledIdentifier(_, level), _, _, _, _) => levelStack.pop
      case _                                                       =>
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
