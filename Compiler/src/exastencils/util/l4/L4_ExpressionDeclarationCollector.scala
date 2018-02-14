package exastencils.util.l4

import scala.collection.mutable._

import exastencils.base.l4._
import exastencils.baseExt.l4._
import exastencils.core.collectors.Collector
import exastencils.datastructures._
import exastencils.layoutTransformation.l4._

class L4_ExpressionDeclarationCollector extends Collector {
  var plainDeclarations = ListBuffer[HashMap[String, L4_ExpressionDeclaration]]()
  var leveledDeclarations = ListBuffer[HashMap[(String, Int), L4_ExpressionDeclaration]]()

  // reset collects also declarations from global sections
  override def reset() : Unit = {
    plainDeclarations.clear()
    leveledDeclarations.clear()

    plainDeclarations += HashMap()
    leveledDeclarations += HashMap()

    exastencils.core.StateManager.findAll[L4_GlobalSection]().foreach(_.declarations.foreach {
      case decl : L4_ExpressionDeclaration => addDecl(decl)
      case _                               =>
    })
  }

  def addDecl(decl : L4_ExpressionDeclaration) {
    if (decl.levels.isEmpty)
      plainDeclarations.last += ((decl.name, decl))
    else
      leveledDeclarations.last += (((decl.name, decl.levels.get.resolveLevel), decl))
  }

  def openNewScope() = {
    plainDeclarations += plainDeclarations.last.clone()
    leveledDeclarations += leveledDeclarations.last.clone()
  }

  def closeScope() = {
    plainDeclarations.trimEnd(1)
    leveledDeclarations.trimEnd(1)
  }

  override def enter(node : Node) : Unit = {
    node match {
      // handle global and layout sections as any other scope
      case _ : L4_GlobalSection => openNewScope()
      case _ : L4_LayoutSection => openNewScope()

      case _ : L4_Function          => openNewScope()
      case _ : L4_LoopOverFragments => openNewScope()
      case _ : L4_LoopOverField     => openNewScope()
      case _ : L4_ForLoop           => openNewScope()
      case _ : L4_UntilLoop         => openNewScope()
      case _ : L4_IfCondition       => openNewScope()

      case decl : L4_ExpressionDeclaration => addDecl(decl)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : L4_GlobalSection => closeScope()
      case _ : L4_LayoutSection => closeScope()

      case _ : L4_Function          => closeScope()
      case _ : L4_LoopOverFragments => closeScope()
      case _ : L4_LoopOverField     => closeScope()
      case _ : L4_ForLoop           => closeScope()
      case _ : L4_UntilLoop         => closeScope()
      case _ : L4_IfCondition       => closeScope()

      case _ =>
    }
  }

  def exists(name : String) = plainDeclarations.last.contains(name) || leveledDeclarations.last.keys.exists(_._1 == name)
  def existsPlain(name : String) = plainDeclarations.last.contains(name)
  def existsLeveled(name : String, level : Int) = leveledDeclarations.last.contains((name, level))

  def getDeclaration(name : String) = plainDeclarations.last(name)
  def getDeclaration(name : String, level : Int) = leveledDeclarations.last((name, level))
}
