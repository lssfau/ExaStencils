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

package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.communication.ir._
import exastencils.config.Knowledge
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.datastructures.Transformation.Output
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.fieldlike.ir.IR_IV_AbstractFieldLikeData
import exastencils.prettyprinting.PpStream

/// CUDA_Allocate

case class CUDA_Allocate(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      IR_FunctionCall(IR_ExternalFunctionReference("cudaMalloc"),
        IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(pointer)),
        numElements * IR_SizeOf(datatype)))
  }
}

/// CUDA_AllocateHost

case class CUDA_AllocateHost(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    if (Knowledge.cuda_useZeroCopy) {
      CUDA_CheckError(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaHostAlloc"),
          IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(pointer)),
          numElements * IR_SizeOf(datatype),
          "cudaHostAllocMapped"))
    } else {
      CUDA_CheckError(
        IR_FunctionCall(IR_ExternalFunctionReference("cudaMallocHost"),
          IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(pointer)),
          numElements * IR_SizeOf(datatype)))
    }
  }
}

/// CUDA_AllocateManaged

case class CUDA_AllocateManaged(var pointer : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() : Output[IR_Statement] = {
    CUDA_CheckError(
      IR_FunctionCall(IR_ExternalFunctionReference("cudaMallocManaged"),
        IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(pointer)),
        numElements * IR_SizeOf(datatype)))
  }
}

/// CUDA_Free

case class CUDA_Free(var pointer : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaFree"), pointer))
}

/// CUDA_FreeHost

case class CUDA_FreeHost(var pointer : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaFreeHost"), pointer))
}

/// CUDA_Memcpy

case class CUDA_Memcpy(var dest : IR_Expression, var src : IR_Expression, var sizeInBytes : IR_Expression, var direction : String) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaMemcpy"), dest, src, sizeInBytes, direction))
}

/// CUDA_MemcpyAsync

case class CUDA_MemcpyAsync(var dest : IR_Expression, var src : IR_Expression, var sizeInBytes : IR_Expression, var direction : String, var stream : Option[CUDA_Stream] = None) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaMemcpyAsync"), ListBuffer[IR_Expression](dest, src, sizeInBytes, direction) ++ stream))
}

/// CUDA_MemPrefetch

case class CUDA_MemPrefetch(var pointer : IR_Expression, var sizeInBytes : IR_Expression, var target : String, var stream : Option[CUDA_Stream] = None) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaMemPrefetchAsync"), ListBuffer[IR_Expression](pointer, sizeInBytes, target) ++ stream))
}

/// CUDA_Memset

case class CUDA_Memset(var data : IR_Expression, var value : IR_Expression, var numElements : IR_Expression, var datatype : IR_Datatype) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaMemset"), data, value, numElements * IR_SizeOf(datatype)))
}

/// CUDA_GetDevPointer

case class CUDA_GetDevPointer(var devicePtr : IR_Expression, var hostPtr : IR_Expression) extends CUDA_HostStatement with IR_Expandable {
  override def expand() = CUDA_CheckError(IR_FunctionCall(IR_ExternalFunctionReference("cudaHostGetDevicePointer"),
    IR_Cast(IR_PointerDatatype(IR_PointerDatatype(IR_UnitDatatype)), IR_AddressOf(devicePtr)), hostPtr, 0))
}

/// CUDA_FieldDeviceData

case class CUDA_FieldDeviceData(var field : IR_FieldLike, var slot : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractFieldLikeData(true, false, true, true, false) {
  override var level : IR_Expression = field.level

  override def resolveName() = (if (1 == field.numSlots) s"fieldDeviceData" else "slottedFieldDeviceData") +
    resolvePostfix(fragmentIdx.prettyprint, "", if (Knowledge.data_useFieldNamesAsIdx) field.name else field.index.toString, level.prettyprint, "")

  override def getDtor() : Option[IR_Statement] = {
    val origSlot = slot
    slot = "slot"

    def access = this

    val ret = Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer(
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
    slot = origSlot
    ret
  }
}

/// CUDA_BufferDeviceDataLike

trait CUDA_BufferDeviceDataLike extends IR_IV_AbstractCommBufferLike

/// CUDA_BufferDeviceData

case class CUDA_BufferDeviceData(
    var field : IR_FieldLike,
    var send : Boolean,
    var size : IR_Expression,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_AbstractCommBuffer with CUDA_BufferDeviceDataLike {

  override def resolveName() = s"bufferDevice_${ direction }_${ concurrencyId }" +
    resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)

  override def getDtor() : Option[IR_Statement] = {
    def access = this

    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
  }
}

/// CUDA_MatrixDeviceCopyLike

trait CUDA_MatrixDeviceCopyLike extends IR_InternalVariableLike with IR_Expression {
  def name : String
  def baseDt : IR_Datatype
  def size : IR_Expression
  def fragmentIdx : IR_Expression

  def asFuncArg() = IR_FunctionArgument(resolveName(), resolveDatatype())
  def getAccess() = resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)
  def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDt)

  override def getCtor() : Option[IR_Statement] = Some(wrapInLoops(
    if (Knowledge.cuda_useManagedMemory)
      IR_ArrayAllocation(getAccess(), baseDt, size)
    else
      CUDA_Allocate(getAccess(), size, baseDt)))

  override def getDtor() : Option[IR_Statement] = Some(wrapInLoops(IR_IfCondition(getAccess(),
    if (Knowledge.cuda_useManagedMemory)
      IR_ArrayFree(getAccess())
    else
      CUDA_Free(getAccess()))))
}

/// CUDA_MatrixDeviceCopy

case class CUDA_MatrixDeviceCopy(
    var name : String,
    var baseDt : IR_Datatype,
    var size : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, false, false, false) with CUDA_MatrixDeviceCopyLike {

  def resolveName() : String = name
  override def prettyprint(out : PpStream) : Unit = out << getAccess()
}

/// CUDA_ReductionResultBufferLike

trait CUDA_ReductionResultBufferLike extends IR_InternalVariableLike with IR_Expression {
  def name : String
  def baseDt : IR_Datatype
  def size : IR_Expression
  def fragmentIdx : IR_Expression

  def getAccess() = resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)
  def resolveDatatype() : IR_Datatype = IR_PointerDatatype(baseDt)

  override def getCtor() : Option[IR_Statement] = Some(wrapInLoops(IR_ArrayAllocation(getAccess(), baseDt, size)))
  override def getDtor() : Option[IR_Statement] = Some(wrapInLoops(IR_IfCondition(getAccess(), IR_ArrayFree(getAccess()))))
}

/// CUDA_ReductionResultBuffer

// TODO: temporary solution until the reductions are optimized
case class CUDA_ReductionResultBuffer(
    var name : String,
    var baseDt : IR_Datatype,
    var size : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, false, false, false) with CUDA_ReductionResultBufferLike{

  def resolveName() : String = name
  override def prettyprint(out : PpStream) : Unit = out << getAccess()
}

/// CUDA_ReductionFragmentCopyLike

trait CUDA_ReductionFragmentCopyLike extends IR_InternalVariableLike with IR_Expression {
  def name : String
  def baseDt : IR_Datatype
  def fragmentIdx : IR_Expression

  def getAccess() = resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)
  def resolveDatatype() : IR_Datatype = baseDt match {
    case mat : IR_MatrixDatatype =>
      IR_ArrayDatatype(mat.resolveBaseDatatype, mat.sizeN * mat.sizeM)
    case dt : IR_Datatype        =>
      dt
  }
}

/// CUDA_ReductionFragmentCopy

case class CUDA_ReductionFragmentCopy(
    var name : String,
    var baseDt : IR_Datatype,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_InternalVariable(true, false, false, false, false) with CUDA_ReductionFragmentCopyLike {

  def resolveName() : String = name
  override def prettyprint(out : PpStream) : Unit = out << getAccess()
}

/// CUDA_AdaptDeviceAccessesForMM

object CUDA_AdaptDeviceAccessesForMM extends DefaultStrategy("Adapt allocations and de-allocations on host and device") {
  this += new Transformation("Adapting", {
    case cudaVariant : CUDA_FieldDeviceData if Knowledge.cuda_useManagedMemory =>
      IR_IV_AbstractFieldLikeData(cudaVariant.field, cudaVariant.slot, cudaVariant.fragmentIdx)

    case cudaVariant : CUDA_BufferDeviceData if Knowledge.cuda_useManagedMemory =>
      IR_IV_CommBuffer(cudaVariant.field, cudaVariant.send, cudaVariant.size, cudaVariant.neighIdx,
        cudaVariant.concurrencyId, cudaVariant.indexOfRefinedNeighbor, cudaVariant.fragmentIdx)
  })
}

/// CUDA_AdaptAllocations

object CUDA_AdaptAllocations extends DefaultStrategy("Adapt allocations and de-allocations on host and device") {
  var fieldHostAllocations = ListBuffer[IR_FieldLike]()
  var bufferHostAllocations = ListBuffer[IR_FieldLike]()

  this.onBefore = () => {
    fieldHostAllocations.clear()
    bufferHostAllocations.clear()
  }

  this += new Transformation("Scanning host allocations", {
    case alloc @ IR_ArrayAllocation(pointer : IR_IV_AbstractFieldLikeData, _, _) =>
      fieldHostAllocations += pointer.field
      alloc
    case alloc @ IR_ArrayAllocation(pointer : IR_IV_CommBufferLike, _, _)            =>
      bufferHostAllocations += pointer.field
      alloc
  })

  this += new Transformation("Adapting", {
    case alloc @ CUDA_Allocate(fieldData : CUDA_FieldDeviceData, _, _) if Knowledge.cuda_useZeroCopy && fieldHostAllocations.contains(fieldData.field) =>
      CUDA_GetDevPointer(alloc.pointer, IR_IV_AbstractFieldLikeData(fieldData.field, fieldData.slot, fieldData.fragmentIdx))

    case alloc @ CUDA_Allocate(bufferData : CUDA_BufferDeviceData, _, _) if Knowledge.cuda_useZeroCopy && bufferHostAllocations.contains(bufferData.field) =>
      CUDA_GetDevPointer(alloc.pointer,
        IR_IV_CommBuffer(bufferData.field, bufferData.send, bufferData.size, bufferData.neighIdx,
          bufferData.concurrencyId, bufferData.indexOfRefinedNeighbor, bufferData.fragmentIdx))

    case CUDA_Free(fieldData : CUDA_FieldDeviceData) if Knowledge.cuda_useZeroCopy && fieldHostAllocations.contains(fieldData.field) =>
      IR_NullStatement

    case CUDA_Free(bufferData : CUDA_BufferDeviceData) if Knowledge.cuda_useZeroCopy && bufferHostAllocations.contains(bufferData.field) =>
      IR_NullStatement
  })

  this += new Transformation("Adapting", {
    case alloc @ IR_ArrayAllocation(pointer, datatype, size) =>
      if (Knowledge.cuda_usePinnedHostMemory)
        CUDA_AllocateHost(pointer, size, datatype)
      else if (Knowledge.cuda_useManagedMemory)
        CUDA_AllocateManaged(pointer, size, datatype)
      else
        alloc

    case free @ IR_ArrayFree(pointer) =>
      if (Knowledge.cuda_usePinnedHostMemory)
        CUDA_FreeHost(pointer)
      else if (Knowledge.cuda_useManagedMemory)
        CUDA_Free(pointer)
      else
        free
  })
}
