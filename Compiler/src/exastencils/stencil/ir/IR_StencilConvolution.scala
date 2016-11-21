package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir.IR_FieldAccess
import exastencils.operator.ir.IR_Stencil
import exastencils.optimization.ir.IR_GeneralSimplify
import exastencils.util.ir.IR_ResultingDatatype

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilConvolution

case class IR_StencilConvolution(var stencil : IR_Stencil, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(stencil.datatype, fieldAccess.datatype)

  def resolveEntry(idx : Int) : IR_Expression = {
    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(stencil.entries(idx).offset)
    while (offset.length < fieldAccess.index.length)
      offset.indices :+= IR_IntegerConstant(0)

    stencil.entries(idx).coefficient * IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    IR_GeneralSimplify.doUntilDoneStandalone(ret)
    ret
  }
}

/// IR_StencilFieldConvolution

case class IR_StencilFieldConvolution(var stencilFieldAccess : IR_StencilFieldAccess, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = IR_ResultingDatatype(stencilFieldAccess.datatype, fieldAccess.datatype)

  def resolveEntry(idx : Int) : IR_Expression = {
    val stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    // fill offset with zeros to match dimensionality of the field access
    val offset = Duplicate(stencilFieldAccess.stencilFieldSelection.offsets(idx))
    while (offset.length < fieldAccess.index.length)
      offset.indices :+= IR_IntegerConstant(0)

    IR_FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencilFieldAccess.stencilFieldSelection.offsets.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    IR_GeneralSimplify.doUntilDoneStandalone(ret)
    ret
  }
}
