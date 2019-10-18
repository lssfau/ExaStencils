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

package exastencils.operator.l3

import exastencils.base.l3.L3_MayBlockResolution
import exastencils.datastructures._
import exastencils.knowledge.l3._
import exastencils.logger._

/// L3_StencilDecl

abstract class L3_StencilDecl extends L3_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L3 stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L3_PrepareStencilDeclaration

object L3_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L3 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L3_StencilDecl =>
      L3_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L3_ProcessStencilDeclaration

object L3_ProcessStencilDeclarations extends DefaultStrategy("Integrate L3 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L3_StencilDecl if L3_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
