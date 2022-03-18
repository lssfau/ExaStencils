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

package exastencils.field.l4

import exastencils.field.ir._
import exastencils.fieldlike.l4.L4_FieldLikeCollection
import exastencils.knowledge.l4.L4_KnowledgeContainer._
import exastencils.knowledge.l4._

/// L4_FieldCollection

object L4_FieldCollection extends L4_FieldLikeCollection[L4_Field, IR_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_PrepareDeclarations.strategies += L4_PrepareFieldDeclarations
  L4_ProcessDeclarations.strategies += L4_ProcessFieldDeclarations

  L4_PrepareAccesses.strategies += L4_PrepareFieldAccesses
  L4_ResolveAccesses.strategies += L4_ResolveFieldAccesses

  override def name = "L4_FieldCollection"
  override def progress() = objects.foreach(obj => IR_FieldCollection.add(obj.progress()))
}
