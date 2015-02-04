package exastencils.core.collectors

import exastencils.datastructures._
import exastencils.datastructures.l4._
import exastencils.logger._

class L4LevelCollector extends Collector {
  private var curLevel = -1

  override def enter(node : Node) : Unit = {
    node match {
      case FunctionStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _, _, _) => curLevel = level
      case StencilDeclarationStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _) => curLevel = level
      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case FunctionStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _, _, _) => // due to duplication of functions, functions can be left that were never entered
      case StencilDeclarationStatement(LeveledIdentifier(_, SingleLevelSpecification(level)), _) => // 
      case _ =>
    }
  }

  override def reset() : Unit = {
    curLevel = -1
  }

  def getCurrentLevel : Int = {
    if (curLevel < 0)
      Logger.dbg("Trying to access level outside of a valid scope")
    curLevel
  }
}
