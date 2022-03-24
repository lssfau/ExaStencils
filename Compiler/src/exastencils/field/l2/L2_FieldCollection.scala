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

package exastencils.field.l2

import exastencils.field.l3._
import exastencils.fieldlike.l2.L2_FieldLikeCollection
import exastencils.fieldlike.l2.L2_FieldLikeCollections
import exastencils.knowledge.l2._

/// L2_FieldCollection

object L2_FieldCollection extends L2_FieldLikeCollection[L2_Field, L3_Field] {
  exastencils.core.Duplicate.registerConstant(this)

  L2_FieldLikeCollections.register(this)
  L2_KnowledgeContainer.register(this)

  override def name = "L2_FieldCollection"
  override def progress() = objects.foreach(obj => L3_FieldCollection.add(obj.progress()))
}
