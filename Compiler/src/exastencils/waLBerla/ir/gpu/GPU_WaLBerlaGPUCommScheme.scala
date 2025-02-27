package exastencils.waLBerla.ir.gpu

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.logger.Logger
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommScheme
import exastencils.waLBerla.ir.communication.IR_WaLBerlaInitCommSchemes
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

@deprecated
case class GPU_WaLBerlaGPUCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaCommScheme {

  def basetype = IR_UniquePointerDatatype(WB_CommScheme(onGPU = true))

  def createUniformPackInfo() = {
    Logger.warn("The 'MemcpyPackInfo' classes for waLBerla's GPU communication schemes may not yield correct results.")
    make_shared(s"gpu::communication::MemcpyPackInfo< ${ WB_FieldDatatype(wbField, onGPU = true).prettyprint() } >", blockDataID)
  }

  def name = s"gpuCommScheme_${ wbField.name }"

  def blockDataID = IR_WaLBerlaBlockDataID(wbField, slot, onGPU = true)
  def blockForest = IR_WaLBerlaBlockForest()

  override def isPrivate : Boolean = true

  // init comm scheme function sets up comm scheme for all levels and slots
  override def getCtor() : Option[IR_Statement] = Some(
    IR_FunctionCall(IR_WaLBerlaInitCommSchemes(onGPU = false, wbField).name)
  )
}