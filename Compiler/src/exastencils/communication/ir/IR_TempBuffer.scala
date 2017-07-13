package exastencils.communication.ir

import scala.collection.mutable.StringBuilder

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_Linearization
import exastencils.datastructures._
import exastencils.polyhedron.IR_PolyArrayAccessLike

/// IR_TempBufferAccess

case class IR_TempBufferAccess(var buffer : IR_IV_CommBuffer, var index : IR_ExpressionIndex, var strides : IR_ExpressionIndex) extends IR_Expression with IR_SpecialExpandable with IR_PolyArrayAccessLike {
  override def datatype = buffer.datatype

  override def uniqueID : String = {
    val name = new StringBuilder("buffer")
    name.append('_').append(buffer.direction)
    name.append('_').append(buffer.field.name).append(buffer.field.index).append('_').append(buffer.field.level)
    name.append("_n").append(buffer.neighIdx.prettyprint())
    name.append("_f").append(buffer.fragmentIdx.prettyprint())
    name.toString()
  }

  // use Knowledge.data_alignTmpBufferPointers for alignedAccessPossible if aligned vector operations are possible for tmp buffers
  def linearize = IR_ArrayAccess(buffer, IR_Linearization.linearizeIndex(index, strides), alignedAccessPossible = false)
}

/// IR_LinearizeTempBufferAccess

object IR_LinearizeTempBufferAccess extends DefaultStrategy("Linearize TempBufferAccess nodes") {
  this += new Transformation("Linearize", {
    case access : IR_TempBufferAccess => access.linearize
  })
}
