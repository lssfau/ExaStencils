package exastencils.util.l2

import scala.collection.mutable._

import exastencils.base.l2._
import exastencils.baseExt.l2._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class L2_VariableDeclarationCollector extends Collector {
  var plainDeclarations = ListBuffer[HashMap[String, L2_VariableDeclaration]]()
  var leveledDeclarations = ListBuffer[HashMap[(String, Int), L2_VariableDeclaration]]()

  // reset collects also declarations from global sections
  override def reset() : Unit = {
    plainDeclarations.clear()
    leveledDeclarations.clear()

    plainDeclarations += HashMap()
    leveledDeclarations += HashMap()
    exastencils.core.StateManager.findAll[L2_GlobalSection]().foreach(_.declarations.foreach(addDecl))
  }

  def addDecl(decl : L2_VariableDeclaration) {
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
      case _ : L2_GlobalSection => openNewScope()

      case decl : L2_VariableDeclaration => addDecl(decl)

      case _ =>
    }
  }

  override def leave(node : Node) : Unit = {
    node match {
      case _ : L2_GlobalSection => closeScope()

      case _ =>
    }
  }

  def exists(name : String) = plainDeclarations.last.contains(name) || leveledDeclarations.last.keys.exists(_._1 == name)
  def existsPlain(name : String) = plainDeclarations.last.contains(name)
  def existsLeveled(name : String, level : Int) = leveledDeclarations.last.contains((name, level))

  def getDeclaration(name : String) = plainDeclarations.last(name)
  def getDeclaration(name : String, level : Int) = leveledDeclarations.last((name, level))
}
