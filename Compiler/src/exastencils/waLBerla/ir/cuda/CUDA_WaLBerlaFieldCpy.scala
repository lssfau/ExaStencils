package exastencils.waLBerla.ir.cuda

import exastencils.base.ir._
import exastencils.datastructures.Transformation.OutputType
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes

case class CUDA_WaLBerlaFieldCpy(var dstID : IR_WaLBerlaBlockDataID, var srcID : IR_WaLBerlaBlockDataID) extends IR_Statement with IR_Expandable {

  override def expand() : OutputType = {
    val dstType = IR_WaLBerlaDatatypes.WB_FieldDatatype(dstID.wbField, dstID.onGPU)
    val srcType = IR_WaLBerlaDatatypes.WB_FieldDatatype(srcID.wbField, srcID.onGPU)
    val blocks = IR_WaLBerlaBlockForest()

    IR_ExpressionStatement(IR_FunctionCall(IR_ExternalFunctionReference(s"cuda::fieldCpy< ${dstType.prettyprint()}, ${srcType.prettyprint()} >"), blocks, dstID, srcID))
  }
}
