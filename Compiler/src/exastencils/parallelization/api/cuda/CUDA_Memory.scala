package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.field.ir._

/// CUDA_Allocate

case class CUDA_Allocate(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      IR_FunctionCall("cudaMalloc",
        IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(pointer)),
        numElements * IR_SizeOf(datatype)))
  }
}

/// CUDA_Free

case class CUDA_Free(var pointer : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = IR_ExpressionStatement(IR_FunctionCall("cudaFree", pointer))
}

/// CUDA_Memcpy

case class CUDA_Memcpy(var dest : IR_Expression, var src : IR_Expression, var sizeInBytes : IR_Expression, var direction : String) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaMemcpy", dest, src, sizeInBytes, direction))
}

/// CUDA_Memset

case class CUDA_Memset(var data : IR_Expression, var value : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall("cudaMemset", data, value, numElements * IR_SizeOf(datatype)))
}

/// CUDA_FieldDeviceData

case class CUDA_FieldDeviceData(override var field : IR_Field, override var slot : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldData {
  override var level : IR_Expression = field.level

  override def resolveName() = (if (1 == field.numSlots) s"fieldDeviceData" else "slottedFieldDeviceData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"
    val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_LoopOverDomains.defIt, IR_LoopOverFields.defIt, IR_LoopOverLevels.defIt, IR_LoopOverNeighbors.defIt)

    val ret = Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer(
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
    slot = origSlot
    ret
  }
}

/// CUDA_BufferDeviceData

case class CUDA_BufferDeviceData(override var field : IR_Field, override var direction : String, override var size : IR_Expression, override var neighIdx : IR_Expression, override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractCommBuffer {
  override def resolveName() = s"bufferDevice_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def getDtor() : Option[IR_Statement] = {
    def access = resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)
    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
  }
}
