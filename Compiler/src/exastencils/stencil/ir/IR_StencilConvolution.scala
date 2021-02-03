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

package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldAccess
import exastencils.operator.ir._
import exastencils.optimization.ir._
import exastencils.solver.ir.IR_ResolveIntergridIndices
import exastencils.util.ir.IR_ResultingDatatype

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilConvolution

case class IR_StencilConvolution(var left : IR_StencilAccess, var right : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def stencil = left.target

  def resolveEntry(idx : Int) : IR_Expression = {
    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(stencil.entries(idx)).asStencilOffsetEntry.offset
    while (offset.length < right.index.length)
      offset.indices :+= 0

    val coeff = IR_ExpressionStatement(Duplicate(stencil.entries(idx).coefficient))
    if (left.offset.isDefined) {
      IR_OffsetAllApplicable.offset = left.offset.get
      IR_OffsetAllApplicable.applyStandalone(coeff)
    }


    val level = if (Knowledge.useFasterExpand) IR_ExpandInOnePass.collector.getCurrentLevel else IR_Expand.collector.getCurrentLevel

    IR_ResolveIntergridIndices.overrideLevel = Some(level)
    IR_ResolveIntergridIndices.applyStandalone(coeff)
    IR_ResolveIntergridIndices.overrideLevel = None

    coeff.expression * Duplicate(IR_FieldAccess(right.field, Duplicate(right.slot), right.index + offset, None, false, right.matIndex))

  }

  override def expand() : Output[IR_Expression] = {
    val ret = stencil.entries.indices.map(idx => resolveEntry(idx)).reduceLeft(_ + _)
    IR_GeneralSimplifyWrapper.process[IR_Expression](ret)
  }
}

/// IR_StencilFieldConvolution

case class IR_StencilFieldConvolution(var left : IR_StencilFieldAccess, var right : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def resolveEntry(idx : Int) : IR_Expression = {
    val stencilFieldIdx = Duplicate(left.index)
    stencilFieldIdx.indices :+= (idx : IR_Expression)
    // HACK: honor matrix dt
    stencilFieldIdx.indices :+= (0 : IR_Expression)

    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(left.stencilField.stencil.entries(idx)).asStencilOffsetEntry.offset
    while (offset.length < right.index.length)
      offset.indices :+= 0

    IR_FieldAccess(left.field, Duplicate(left.slot), Duplicate(left.fragIdx), stencilFieldIdx) *
      IR_FieldAccess(right.field, Duplicate(right.slot), Duplicate(right.fragIdx), right.index + offset, None, false, right.matIndex)
  }

  override def expand() : Output[IR_Expression] = {
    val ret = left.stencilField.stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    IR_GeneralSimplifyWrapper.process[IR_Expression](ret)
  }
}
