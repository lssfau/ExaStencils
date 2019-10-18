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

import exastencils.field.l2._
import exastencils.knowledge.l1.L1_KnowledgeContainer._
import exastencils.knowledge.l1._

/// L1_FieldCollection

object L1_FieldCollection extends L1_LeveledKnowledgeCollection[L1_Field, L2_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_PrepareDeclarations.strategies += L1_PrepareFieldDeclarations
  L1_ProcessDeclarations.strategies += L1_ProcessFieldDeclarations

  L1_PrepareAccesses.strategies += L1_PrepareFieldAccesses
  L1_ResolveAccesses.strategies += L1_ResolveFieldAccesses

  override def name = "L1_FieldCollection"
  override def progress() = objects.foreach(obj => L2_FieldCollection.add(obj.progress()))
}
