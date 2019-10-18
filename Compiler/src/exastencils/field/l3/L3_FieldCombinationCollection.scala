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

package exastencils.field.l3

import exastencils.field.l4._
import exastencils.knowledge.l3.L3_KnowledgeContainer.L3_ProcessDeclarations
import exastencils.knowledge.l3._

/// L3_FieldCombinationCollection

object L3_FieldCombinationCollection extends L3_LeveledKnowledgeCollection[L3_FieldCombination, L4_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L3_KnowledgeContainer.register(this)

  L3_ProcessDeclarations.strategies += L3_ProcessFieldCombinationDeclarations

  override def name = "L3_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => L4_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L3_Field) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L3_Field, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L3_Field) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L3_Field, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
