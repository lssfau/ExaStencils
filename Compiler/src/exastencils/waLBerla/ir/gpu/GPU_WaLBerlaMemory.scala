package exastencils.waLBerla.ir.gpu

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.ir.IR_IV_CommBufferLike
import exastencils.fieldlike.ir.IR_FieldLike
import exastencils.parallelization.api.cuda.CUDA_Free
import exastencils.waLBerla.ir.communication.IR_WaLBerlaAbstractCommBuffer


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
    var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaAbstractCommBuffer with IR_IV_CommBufferLike {

  override def basePtr : IR_Expression = GPU_WaLBerlaBufferDeviceDataBasePtr(field, send, size, neighIdx, concurrencyId, indexOfRefinedNeighbor, fragmentIdx)

  override def isPrivate : Boolean = true

  override def name : String = s"wbBufferDevice_${ direction }_${ concurrencyId }" +
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
