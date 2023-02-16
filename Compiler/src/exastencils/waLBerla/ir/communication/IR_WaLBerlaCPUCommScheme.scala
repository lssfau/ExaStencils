package exastencils.waLBerla.ir.communication

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_CommScheme
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_FieldDatatype
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil.make_shared

case class IR_WaLBerlaCPUCommScheme(var wbField : IR_WaLBerlaField, var slot : IR_Expression) extends IR_WaLBerlaCommScheme {

  def basetype = IR_UniquePointerDatatype(WB_CommScheme(onGPU = false))

  def createUniformPackInfo() =
    make_shared(s"field::communication::PackInfo< ${ WB_FieldDatatype(wbField, onGPU = false).prettyprint() } >", blockDataID)

  def name = s"cpuCommScheme_${ wbField.name }"

  def blockDataID = IR_WaLBerlaBlockDataID(wbField, slot, onGPU = false)
  def blockForest = IR_WaLBerlaBlockForest()

  override def isPrivate : Boolean = true

  override def getCtor() : Option[IR_Statement] = Some(
    IR_FunctionCall(IR_WaLBerlaInitCommSchemes(onGPU = false, wbField).name)
  )
}
