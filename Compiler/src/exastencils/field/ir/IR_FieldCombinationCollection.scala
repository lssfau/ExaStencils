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

package exastencils.field.ir

import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.knowledge.ir._

/// IR_FieldCombinationCollection

object IR_FieldCombinationCollection extends IR_LeveledKnowledgeCollection[IR_FieldCombination] {
  exastencils.core.Duplicate.registerConstant(this)

  def existsInCombination(field : IR_FieldLike) = { objects.exists(_.fields.contains(field)) }
  def existsInCombination(field : IR_FieldLike, combType : String) = { objects.exists(c => combType == c.combinationType && c.fields.contains(field)) }

  def getByFieldInCombination(field : IR_FieldLike) = { objects.filter(_.fields.contains(field)) }
  def getByFieldInCombination(field : IR_FieldLike, combType : String) = { objects.filter(c => combType == c.combinationType && c.fields.contains(field)) }
}
