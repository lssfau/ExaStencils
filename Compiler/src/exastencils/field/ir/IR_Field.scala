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

import exastencils.base.ir.IR_ConstIndex
import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_ExpressionIndex
import exastencils.base.ir.IR_MatIndex
import exastencils.baseExt.ir.IR_MatShape
import exastencils.boundary.ir.IR_BoundaryCondition
import exastencils.core.Duplicate
import exastencils.domain.ir.IR_Domain
import exastencils.fieldlike.ir.IR_DirectFieldLikeAccess
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_FieldLikeAccess
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.fieldlike.ir.IR_LinearizedFieldLikeAccess

/// IR_Field

case class IR_Field(
    var name : String,
    var level : Int,
    var index : Int,
    var domain : IR_Domain,
    var codeName : String,
    var layout : IR_FieldLayout,
    var numSlots : Int,
    var boundary : IR_BoundaryCondition,
    var matShape: Option[IR_MatShape],
    var gpuCompatible : Boolean = true
) extends IR_FieldLike {

  override def createDuplicate() : IR_Field = {
    IR_Field.tupled(Duplicate(IR_Field.unapply(this).get))
  }

  override def getFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean, matIndex : Option[IR_MatIndex]) : IR_FieldLikeAccess = {
    IR_FieldAccess(this, slot, fragIdx, index, offset, frozen, matIndex)
  }

  override def getDirectFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_ExpressionIndex) : IR_DirectFieldLikeAccess = {
    IR_DirectFieldAccess(this, slot, fragIdx, index)
  }

  override def getLinearizedFieldAccess(slot : IR_Expression, fragIdx : IR_Expression, index : IR_Expression) : IR_LinearizedFieldLikeAccess = {
    IR_LinearizedFieldAccess(this, slot, fragIdx, index)
  }

  override def getFieldData(slot : IR_Expression, fragIdx : IR_Expression) : IR_IV_AbstractFieldLikeData = {
    IR_IV_FieldData(this, slot, fragIdx)
  }
}
