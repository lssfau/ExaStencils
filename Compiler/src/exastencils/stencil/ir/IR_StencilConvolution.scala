package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldAccess
import exastencils.operator.ir.IR_OffsetAccesses
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.util.ir.IR_ResultingDatatype

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilConvolution

case class IR_StencilConvolution(var left : IR_StencilAccess, var right : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def stencil = left.stencil

  def resolveEntry(idx : Int) : IR_Expression = {
    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(stencil.entries(idx).offset)
    while (offset.length < right.index.length)
      offset.indices :+= IR_IntegerConstant(0)

    val coeff = Duplicate(stencil.entries(idx).coefficient)
    if (left.offset.isDefined) {
      IR_OffsetAccesses.offset = left.offset.get
      IR_OffsetAccesses.applyStandalone(IR_ExpressionStatement(coeff))
    }

    coeff * Duplicate(IR_FieldAccess(right.fieldSelection, right.index + offset))
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencil.entries.indices.view.map(idx => resolveEntry(idx)).reduceLeft(_ + _)
    IR_GeneralSimplify.doUntilDoneStandalone(ret)
    ret
  }
}

/// IR_StencilFieldConvolution

case class IR_StencilFieldConvolution(var left : IR_StencilFieldAccess, var right : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)

  def resolveEntry(idx : Int) : IR_Expression = {
    val stencilFieldIdx = Duplicate(left.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(left.stencilFieldSelection.offsets(idx))
    while (offset.length < right.index.length)
      offset.indices :+= IR_IntegerConstant(0)

    IR_FieldAccess(left.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      IR_FieldAccess(right.fieldSelection, right.index + offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = left.stencilFieldSelection.offsets.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    IR_GeneralSimplify.doUntilDoneStandalone(ret)
    ret
  }
}
