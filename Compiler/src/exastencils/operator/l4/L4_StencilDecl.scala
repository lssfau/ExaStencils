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

package exastencils.operator.l4

import exastencils.base.l4.L4_MayBlockResolution
import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger._

/// L4_StencilDecl

abstract class L4_StencilDecl extends L4_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L4 stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L4_PrepareStencilDeclaration

object L4_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L4 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L4_StencilDecl =>
      L4_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L4_ProcessStencilDeclaration

object L4_ProcessStencilDeclarations extends DefaultStrategy("Integrate L4 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L4_StencilDecl if L4_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
