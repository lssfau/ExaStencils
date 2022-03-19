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
import exastencils.fieldlike.l4.L4_FieldLayoutLikeCollection
import exastencils.knowledge.l4._

/// L4_FieldLayoutCollection

object L4_FieldLayoutCollection extends L4_FieldLayoutLikeCollection[L4_FieldLayout, IR_FieldLayout] {
  exastencils.core.Duplicate.registerConstant(this)

  L4_KnowledgeContainer.register(this)

  override def name = "L4_FieldLayoutCollection"
  override def progress() = for (obj <- objects) IR_FieldLayoutCollection.objects += obj.progress
}
