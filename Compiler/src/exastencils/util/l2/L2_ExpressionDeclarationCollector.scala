//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

package exastencils.util.l2

import scala.collection.mutable._

import exastencils.baseExt.l2._
import exastencils.core.collectors.Collector
import exastencils.datastructures._

class L2_ExpressionDeclarationCollector extends Collector {
  var plainDeclarations = ListBuffer[HashMap[String, L2_ExpressionDeclaration]]()
  var leveledDeclarations = ListBuffer[HashMap[(String, Int), L2_ExpressionDeclaration]]()

  // reset collects also declarations from global sections
  override def reset() : Unit = {
    plainDeclarations.clear()
    leveledDeclarations.clear()

    plainDeclarations += HashMap()
    leveledDeclarations += HashMap()

    exastencils.core.StateManager.findAll[L2_GlobalSection]().foreach(_.declarations.foreach {
      case decl : L2_ExpressionDeclaration => addDecl(decl)
      case _                               =>
    })
  }

  def addDecl(decl : L2_ExpressionDeclaration) {
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

      case decl : L2_ExpressionDeclaration => addDecl(decl)

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
