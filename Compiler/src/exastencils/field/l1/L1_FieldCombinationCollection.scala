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
import exastencils.knowledge.l1.L1_KnowledgeContainer.L1_ProcessDeclarations
import exastencils.knowledge.l1._

/// L1_FieldCombinationCollection

object L1_FieldCombinationCollection extends L1_LeveledKnowledgeCollection[L1_FieldCombination, L2_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L1_KnowledgeContainer.register(this)

  L1_ProcessDeclarations.strategies += L1_ProcessFieldCombinationDeclarations

  override def name = "L1_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => L2_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L1_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L1_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L1_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L1_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
