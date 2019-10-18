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

package exastencils.interfacing.l4

import exastencils.interfacing.ir._
import exastencils.knowledge.l4._

/// L4_ExternalFieldCollection

object L4_ExternalFieldCollection extends L4_LeveledKnowledgeCollection[L4_ExternalField, IR_ExternalField] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

//  L4_PrepareDeclarations.strategies += L4_PrepareExternalFieldDeclarations
//  L4_ProcessDeclarations.strategies += L4_ProcessExternalFieldDeclarations
//
//  L4_PrepareAccesses.strategies += L4_PrepareExternalFieldAccesses
//  L4_ResolveAccesses.strategies += L4_ResolveExternalFieldAccesses

  override def name = "L4_ExternalFieldCollection"
  override def progress() = objects.foreach(obj => IR_ExternalFieldCollection.add(obj.progress()))
}
