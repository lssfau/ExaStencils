package exastencils.stencil.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures.ir.GetResultingDatatype
import exastencils.field.ir.IR_FieldAccess
import exastencils.knowledge._
import exastencils.prettyprinting.PpStream
import exastencils.strategies.SimplifyStrategy

// TODO: is it really necessary to wrap convolutions in separate nodes?
// TODO: update convolutions with new dimensionality logic

/// IR_StencilConvolution

case class IR_StencilConvolution(var stencil : Stencil, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencil.datatype, fieldAccess.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def resolveEntry(idx : Int) : IR_Expression = {
    stencil.entries(idx).coefficient * IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencil.entries(idx).offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}

/// IR_StencilFieldConvolution

case class IR_StencilFieldConvolution(var stencilFieldAccess : IR_StencilFieldAccess, var fieldAccess : IR_FieldAccess) extends IR_Expression with IR_Expandable {
  override def datatype = GetResultingDatatype(stencilFieldAccess.datatype, fieldAccess.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  def resolveEntry(idx : Int) : IR_Expression = {
    val stencilFieldIdx = Duplicate(stencilFieldAccess.index)
    stencilFieldIdx(Knowledge.dimensionality) = idx

    IR_FieldAccess(stencilFieldAccess.stencilFieldSelection.toFieldSelection, stencilFieldIdx) *
      IR_FieldAccess(fieldAccess.fieldSelection, fieldAccess.index + stencilFieldAccess.stencilFieldSelection.stencil.entries(idx).offset)
  }

  override def expand() : Output[IR_Expression] = {
    val ret : IR_Expression = stencilFieldAccess.stencilFieldSelection.stencil.entries.indices.view.map(idx => Duplicate(resolveEntry(idx))).reduceLeft(_ + _)
    SimplifyStrategy.doUntilDoneStandalone(ret)
    ret
  }
}
