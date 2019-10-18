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

package exastencils.operator.l2

import exastencils.base.l2.L2_MayBlockResolution
import exastencils.datastructures._
import exastencils.knowledge.l2._
import exastencils.logger._

/// L2_StencilDecl

abstract class L2_StencilDecl extends L2_LeveledKnowledgeDecl {
  override def progress = Logger.error(s"Trying to progress L2 stencil declaration for stencil $name; this is not supported")
  def addToKnowledge() : Unit
}

/// L2_PrepareStencilDeclaration

object L2_PrepareStencilDeclarations extends DefaultStrategy("Prepare knowledge for L2 stencils") {
  this += Transformation("Process new stencils", {
    case decl : L2_OperatorFromEquation =>
      decl.addDeclarations()
      decl // preserve declaration statement

    case decl : L2_StencilDecl =>
      L2_StencilCollection.addDeclared(decl.name, decl.levels)
      decl // preserve declaration statement
  })
}

/// L2_ProcessStencilDeclaration

object L2_ProcessStencilDeclarations extends DefaultStrategy("Integrate L2 stencil declarations with knowledge") {
  this += Transformation("Process new stencils", {
    case decl : L2_StencilDecl if L2_MayBlockResolution.isDone(decl) =>
      decl.addToKnowledge()
      None // consume declaration statement
  })
}
