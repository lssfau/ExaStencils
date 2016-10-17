package exastencils.communication

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.datastructures._
import exastencils.datastructures.ir.iv
import exastencils.prettyprinting.PpStream

/// IR_TempBufferAccess

case class IR_TempBufferAccess(var buffer : iv.TmpBuffer, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = buffer.datatype
  override def prettyprint(out : PpStream) : Unit = out << "\n --- NOT VALID ; NODE_TYPE = " << this.getClass.getName << "\n"

  // use Knowledge.data_alignTmpBufferPointers for alignedAccessPossible if aligned vector operations are possible for tmp buffers
  def linearize = IR_ArrayAccess(buffer, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeTempBufferAccess

object IR_LinearizeTempBufferAccess extends DefaultStrategy("Linearize TempBufferAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_TempBufferAccess => access.linearize
  })
}
