package exastencils.util.l3

import scala.collection.mutable._

import exastencils.base.l3._
import exastencils.baseExt.l3._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class L3_VariableDeclarationCollector extends Collector {
  var plainDeclarations = ListBuffer[HashMap[String, L3_VariableDeclaration]]()
  var leveledDeclarations = ListBuffer[HashMap[(String, Int), L3_VariableDeclaration]]()

  // reset collects also declarations from global sections
  override def reset() : Unit = {
    plainDeclarations.clear()
    leveledDeclarations.clear()

    plainDeclarations += HashMap()
    leveledDeclarations += HashMap()
    exastencils.core.StateManager.findAll[L3_GlobalSection]().foreach(_.declarations.foreach(addDecl))
  }

  def addDecl(decl : L3_VariableDeclaration) {
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
      // handle global sections as any other scope
      case _ : L3_GlobalSection => openNewScope()

      case _ : L3_Function    => openNewScope()
      case _ : L3_ForLoop     => openNewScope()
      case _ : L3_UntilLoop   => openNewScope()
      case _ : L3_IfCondition => openNewScope()

      case decl : L3_VariableDeclaration => addDecl(decl)
      case decl : L3_Function.Argument   => addDecl(L3_VariableDeclaration(decl.name, None, decl.datatype, None, false))

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : L3_GlobalSection => closeScope()

      case _ : L3_Function    => closeScope()
      case _ : L3_ForLoop     => closeScope()
      case _ : L3_UntilLoop   => closeScope()
      case _ : L3_IfCondition => closeScope()

      case _ =>
    }
  }

  def exists(name : String) = plainDeclarations.last.contains(name) || leveledDeclarations.last.keys.exists(_._1 == name)
  def existsPlain(name : String) = plainDeclarations.last.contains(name)
  def existsLeveled(name : String, level : Int) = leveledDeclarations.last.contains((name, level))

  def getDeclaration(name : String) = plainDeclarations.last(name)
  def getDeclaration(name : String, level : Int) = leveledDeclarations.last((name, level))
}
