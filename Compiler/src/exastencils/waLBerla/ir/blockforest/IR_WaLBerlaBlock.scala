package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

trait IR_WaLBerlaBlockLike extends IR_WaLBerlaBlockLoopVariable {

  import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks._

  override def resolveName() : String = "block"

  def access : IR_VariableAccess = IR_VariableAccess(resolveName(), resolveDatatype())

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(access, IR_ArrayAccess(IR_WaLBerlaLocalBlocks(), defIt))

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

case class IR_WaLBerlaBlock() extends IR_WaLBerlaBlockLike {
  override def resolveDatatype() : IR_Datatype = IR_ConstPointerDatatype(WB_Block)
}

case class IR_WaLBerlaIBlock() extends IR_WaLBerlaBlockLike {
  override def resolveDatatype() : IR_Datatype = IR_ConstPointerDatatype(WB_IBlock)
}
