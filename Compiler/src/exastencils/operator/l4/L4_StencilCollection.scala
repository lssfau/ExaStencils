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

import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._
import exastencils.operator.ir._

/// L4_StencilCollection

object L4_StencilCollection extends L4_LeveledKnowledgeCollection[L4_Stencil, IR_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareStencilDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessStencilDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareStencilAccesses
  L4_ResolveAccesses.strategies += L4_ResolveStencilAccesses

  override def name = "L4_StencilCollection"
  override def progress() = objects.foreach(obj => IR_StencilCollection.add(obj.progress()))
}
