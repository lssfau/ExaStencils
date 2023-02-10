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
import exastencils.fieldlike.l4.L4_FieldLike
import exastencils.knowledge.l4.L4_KnowledgeContainer.L4_ProcessDeclarations
import exastencils.knowledge.l4._

/// L4_FieldCombinationCollection

object L4_FieldCombinationCollection extends L4_LeveledKnowledgeCollection[L4_FieldCombination, IR_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  L4_ProcessDeclarations.strategies += L4_ProcessFieldCombinationDeclarations

  override def name = "L4_FieldCombinationCollection"
  override def progress() = objects.foreach(obj => IR_FieldCombinationCollection.add(obj.progress()))

  def existsInCombination(field : L4_FieldLike[_, _]) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : L4_FieldLike[_, _], combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : L4_FieldLike[_, _]) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : L4_FieldLike[_, _], combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
