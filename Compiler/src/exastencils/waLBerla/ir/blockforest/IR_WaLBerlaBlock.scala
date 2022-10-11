package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

class IR_WaLBerlaBlock(name : String, dt : IR_Datatype) extends IR_VariableAccess(name, dt) {

  // get field data from block
  def getData(blockID : IR_WaLBerlaBlockDataID) = {
    val wbField = blockID.wbField
    val fieldDt = WB_FieldDatatype(wbField)
    new IR_MemberFunctionCallArrowWithDt(this, s"getData< ${ fieldDt.typeName } >", ListBuffer(IR_WaLBerlaBlockDataID(wbField, blockID.slot)), fieldDt)
  }
}
