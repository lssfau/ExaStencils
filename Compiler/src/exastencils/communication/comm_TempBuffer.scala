package exastencils.communication

import exastencils.base.ir._
import exastencils.datastructures.ir.iv
import exastencils.knowledge.Mapping
import exastencils.prettyprinting.PpStream

case class IR_TempBufferAccess(var buffer : iv.TmpBuffer, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression {
  override def datatype = buffer.datatype
  override def prettyprint(out : PpStream) : Unit = out << "NOT VALID ; CLASS = TempBufferAccess\n"

  // use Knowledge.data_alignTmpBufferPointers for alignedAccessPossible if aligned vector operations are possible for tmp buffers
  def linearize = IR_ArrayAccess(buffer, Mapping.resolveMultiIdx(index, strides), alignedAccessPossible = false)
}
