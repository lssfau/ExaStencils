package exastencils.waLBerla.ir.gpu

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverNeighbors
import exastencils.communication.ir.IR_IV_CommBufferLike
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.parallelization.api.cuda._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.communication.IR_WaLBerlaAbstractCommBuffer
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember


/// GPU_WaLBerlaCommBufferBasePtr

case class GPU_WaLBerlaBufferDeviceDataBasePtr(
    override var field : IR_FieldLike,
    override var send : Boolean,
    override var size : IR_Expression,
    override var neighIdx : IR_Expression,
    override var concurrencyId : Int,
    override var indexOfRefinedNeighbor : Option[IR_Expression],
    override var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaAbstractCommBuffer {

  override def name = s"wbBufferDevice_${ direction }_${ concurrencyId }" + "_base"
}

/// GPU_WaLBerlaBufferDeviceData

case class GPU_WaLBerlaBufferDeviceData(
    var field : IR_FieldLike,
    var send : Boolean,
    var size : IR_Expression,
    var neighIdx : IR_Expression,
    var concurrencyId : Int,
    var indexOfRefinedNeighbor : Option[IR_Expression],
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaAbstractCommBuffer with IR_IV_CommBufferLike with CUDA_BufferDeviceDataLike {

  override def basePtr : IR_Expression = GPU_WaLBerlaBufferDeviceDataBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

  override def isPrivate : Boolean = true

  override def name : String = s"wbBufferDevice_${ direction }_${ concurrencyId }"

  override def getDtor() : Option[IR_Statement] = {
    def access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, field.index, field.level, IR_LoopOverNeighbors.defIt)

    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
  }
}

/// GPU_WaLBerlaReductionDeviceData

case class GPU_WaLBerlaReductionDeviceData(
    var numPoints : IR_Expression,
    var targetDt : IR_Datatype,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaInterfaceMember(true, false, false) with CUDA_ReductionDeviceDataLike {

  override def prettyprint(out : PpStream) = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  def baseDt = targetDt.resolveBaseDatatype

  override def resolveDatatype() = IR_PointerDatatype(baseDt)

  override def getDtor() : Option[IR_Statement] = {
    val access = resolveAccess(resolveName(), IR_LoopOverFragments.defIt, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

    Some(wrapInLoops(
      IR_IfCondition(access,
        ListBuffer[IR_Statement](
          CUDA_Free(access),
          IR_Assignment(access, 0)))))
  }

  override def name : String = "wbReductionDeviceData"
  override def isPrivate : Boolean = true
}

/// GPU_WaLBerlaMatrixDeviceCopy

case class GPU_WaLBerlaMatrixDeviceCopy(
    var name : String,
    var baseDt : IR_Datatype,
    var size : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaInterfaceMember(true, false, false) with CUDA_MatrixDeviceCopyLike {

  override def prettyprint(out : PpStream) : Unit = out << getAccess()
  override def isPrivate : Boolean = true
}

/// GPU_WaLBerlaReductionResultBuffer

// TODO: temporary solution until the reductions are optimized
case class GPU_WaLBerlaReductionResultBuffer(
    var name : String,
    var baseDt : IR_Datatype,
    var size : IR_Expression,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaInterfaceMember(true, false, false) with CUDA_ReductionResultBufferLike {

  override def prettyprint(out : PpStream) : Unit = out << getAccess()
  override def isPrivate : Boolean = true
}

/// GPU_WaLBerlaReductionFragmentCopy

case class GPU_WaLBerlaReductionFragmentCopy(
    var name : String,
    var baseDt : IR_Datatype,
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt
) extends IR_WaLBerlaInterfaceMember(true, false, false) with CUDA_ReductionFragmentCopyLike {

  override def prettyprint(out : PpStream) : Unit = out << getAccess()
  override def isPrivate : Boolean = true
}
