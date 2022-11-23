package exastencils.waLBerla.ir.cuda

import exastencils.base.ir.IR_Expression
import exastencils.base.ir.IR_UniquePointerDatatype
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.communication.IR_WaLBerlaCommScheme
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.getGeneratedName
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

case class IR_WaLBerlaGPUCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaCommScheme {

  def basetype = IR_UniquePointerDatatype(WB_CommScheme(onGPU = true))

  def createUniformPackInfo() =
    make_shared(s"cuda::communication::GPUPackInfo< ${ WB_FieldDatatype(wbField, onGPU = true).prettyprint() } >", blockDataID)

  def name = getGeneratedName(s"gpuCommScheme_${ wbField.name }")

  override def prettyprint(out : PpStream) : Unit = out << baseAccess()

  val blockDataID = IR_WaLBerlaBlockDataID(wbField, slot)
  val blockForest = IR_WaLBerlaBlockForest()
}