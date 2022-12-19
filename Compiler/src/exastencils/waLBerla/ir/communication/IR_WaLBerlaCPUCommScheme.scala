package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.getGeneratedName
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

case class IR_WaLBerlaCPUCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaCommScheme {

  def basetype = IR_UniquePointerDatatype(WB_CommScheme())

  def createUniformPackInfo() =
    make_shared(s"field::communication::PackInfo< ${ WB_FieldDatatype(wbField).prettyprint() } >", blockDataID)

  def name = getGeneratedName(s"cpuCommScheme_${ wbField.name }")

  override def prettyprint(out : PpStream) : Unit = out << baseAccess()

  val blockDataID = IR_WaLBerlaBlockDataID(wbField, slot, onGPU = false)
  val blockForest = IR_WaLBerlaBlockForest()
}
