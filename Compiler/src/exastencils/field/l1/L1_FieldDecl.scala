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

package exastencils.field.l1

import exastencils.base.l1.L1_MayBlockResolution
import exastencils.datastructures._
import exastencils.knowledge.l1._
import exastencils.logger._

/// L1_FieldDecl

abstract class L1_FieldDecl extends L1_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L1 field declaration for field $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L1_PrepareFieldDeclaration

object L1_PrepareFieldDeclarations extends DefaultStrategy("Prepare knowledge for L1 fields") {
  this += Transformation("Process new fields", {
    case decl : L1_FieldDecl =>
      L1_FieldCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L1_ProcessFieldDeclarations

object L1_ProcessFieldDeclarations extends DefaultStrategy("Integrate L1 field declarations with knowledge") {
  this += Transformation("Process field declarations", {
    case decl : L1_FieldDecl if L1_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
