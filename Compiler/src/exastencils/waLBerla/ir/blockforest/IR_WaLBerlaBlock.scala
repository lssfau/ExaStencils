package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

class IR_WaLBerlaBlock(name : String, dt : IR_Datatype) extends IR_VariableAccess(name, dt) {

  // get field data from block
  def getData(blockDataID : IR_WaLBerlaBlockDataID) = {
    val wbField = blockDataID.wbField
    val fieldDt = WB_FieldDatatype(wbField, blockDataID.onGPU)
    new IR_MemberFunctionCallArrowWithDt(this, s"getData< ${ fieldDt.typeName } >", ListBuffer(blockDataID), fieldDt)
  }

  def getNeighborId(neighborHoodSectionIdx : IR_Expression, neighborIdx : IR_Expression) =
    IR_MemberFunctionCallArrow(this, "getNeighborId", neighborHoodSectionIdx, neighborIdx)

  def getNeighborProcess(neighborHoodSectionIdx : IR_Expression, neighborIdx : IR_Expression) =
    IR_MemberFunctionCallArrow(this, "getNeighborProcess", neighborHoodSectionIdx, neighborIdx)

  def getId() = IR_MemberFunctionCallArrow(this, "getId")
}
