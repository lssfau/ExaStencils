package exastencils.communication.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.datastructures._

/// IR_TempBufferAccess

case class IR_TempBufferAccess(var buffer : IR_IV_CommBuffer, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable {
  override def datatype = buffer.datatype

  // use Knowledge.data_alignTmpBufferPointers for alignedAccessPossible if aligned vector operations are possible for tmp buffers
  def linearize = IR_ArrayAccess(buffer, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeTempBufferAccess

object IR_LinearizeTempBufferAccess extends DefaultStrategy("Linearize TempBufferAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_TempBufferAccess => access.linearize
  })
}
