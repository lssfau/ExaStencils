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

import exastencils.knowledge.l3.L3_KnowledgeContainer._
import exastencils.knowledge.l3._
import exastencils.operator.l4._

/// L3_StencilCollection

object L3_StencilCollection extends L3_LeveledKnowledgeCollection[L3_Stencil, L4_Stencil] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_PrepareDeclarations.strategies += L3_PrepareStencilDeclarations
  L3_ProcessDeclarations.strategies += L3_ProcessStencilDeclarations

  L3_PrepareAccesses.strategies += L3_PrepareStencilAccesses
  L3_ResolveAccesses.strategies += L3_ResolveStencilAccesses

  override def name = "L3_StencilCollection"
  override def progress() = objects.foreach(obj => L4_StencilCollection.add(obj.progress()))
}
