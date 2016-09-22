package exastencils.core.collectors

import exastencils.base.l4._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldDecl
import exastencils.logger._
import exastencils.stencil.l4.L4_StencilDecl

class L4LevelCollector extends Collector {
  private var curLevel = -1

  override def enter(node : Node) : Unit = {
    node match {
      case L4_Function(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _, _, _, _)     => curLevel = level
      case L4_FieldDecl(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _, _, _, _, _) => curLevel = level
      case L4_StencilDecl(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _)           => curLevel = level
      case _                                                                           =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case L4_Function(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _, _, _, _)     => // due to duplication of functions, functions can be left that were never entered
      case L4_FieldDecl(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _, _, _, _, _) => //
      case L4_StencilDecl(L4_LeveledIdentifier(_, L4_SingleLevel(level)), _)           => //
      case _                                                                           =>
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
