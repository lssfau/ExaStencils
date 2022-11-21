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

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.datastructures._
import exastencils.fieldlike.ir._

/// IR_FieldAccessLike

trait IR_FieldAccessLike extends IR_FieldLikeAccessLike {
  def field : IR_Field
  override def target : IR_Field = field
}

/// IR_MultiDimFieldAccess

trait IR_MultiDimFieldAccess extends IR_FieldAccessLike with IR_MultiDimFieldLikeAccess with IR_SpecialExpandable

/// IR_DirectFieldAccess

object IR_DirectFieldAccess {
  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex) = new IR_DirectFieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
}

case class IR_DirectFieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex) extends IR_MultiDimFieldAccess with IR_DirectFieldLikeAccess {

  def linearize = IR_LinearizedFieldAccess(field, slot, fragIdx, field.layout.linearizeIndex(index))

  override def polyStringBuilderBaseName : String = "field"
}

/// IR_FieldAccess

object IR_FieldAccess {
  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex) = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index)

  def applySpecial(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, matIndex : Option[IR_MatIndex]): IR_FieldAccess = {
    val fa = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index)
    fa.matIndex = matIndex
    fa
  }

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex])
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset)

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean)
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset, frozen)

  def apply(field : IR_Field, slot : IR_Expression, index : IR_ExpressionIndex, offset : Option[IR_ConstIndex], frozen : Boolean, matIndex : Option[IR_MatIndex])
  = new IR_FieldAccess(field, slot, IR_LoopOverFragments.defIt, index, offset, frozen, matIndex)
}

case class IR_FieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_ExpressionIndex,
    var offset : Option[IR_ConstIndex] = None,
    var frozen : Boolean = false,
    var matIndex : Option[IR_MatIndex] = None
) extends IR_FieldLikeAccess with IR_MultiDimFieldAccess {

  def expandSpecial = {
    applyUnresolvedOffset()

    IR_DirectFieldAccess(field, slot, fragIdx, index + field.referenceOffset)
  }

  override def offsetWith(newOffset : IR_ConstIndex) = index += newOffset
}

/// IR_ResolveFieldAccess

object IR_ResolveFieldAccess extends DefaultStrategy("Resolve FieldAccess nodes") {
  this += new Transformation("Resolve", {
    case access : IR_FieldAccess => access.expandSpecial
  })
}

/// IR_LinearizedFieldAccess

case class IR_LinearizedFieldAccess(
    var field : IR_Field,
    var slot : IR_Expression,
    var fragIdx : IR_Expression,
    var index : IR_Expression) extends IR_FieldAccessLike with IR_LinearizedFieldLikeAccess {

  override def expand() = {
    IR_ArrayAccess(
      IR_IV_FieldData(field, slot, fragIdx),
      index,
      Knowledge.data_alignFieldPointers)
  }
}
