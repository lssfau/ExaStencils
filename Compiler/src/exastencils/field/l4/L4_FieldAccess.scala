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

import exastencils.base.ProgressLocation
import exastencils.base.l4._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.field.ir._
import exastencils.fieldlike.l4.L4_FieldLikeAccess

/// L4_FieldAccess

case class L4_FieldAccess(
    var target : L4_Field,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[L4_MatIndex] = None
) extends L4_FieldLikeAccess {

  override def progress : IR_FieldAccess = ProgressLocation {
    val field = target.getProgressedObj()

    val numDims = field.layout.numDimsGrid
    val index = IR_LoopOverDimensions.defIt(numDims)

    val progOffset = if (offset.isDefined) {
      val progressedOffset = offset.get.progress
      while (progressedOffset.indices.length < index.length) progressedOffset.indices :+= 0
      Some(progressedOffset)
    } else {
      None
    }

    IR_FieldAccess(field, L4_FieldLikeAccess.resolveSlot(field, slot), index, progOffset, frozen, if(matIndex.isDefined) Some(matIndex.get.progress) else None)
  }
}
