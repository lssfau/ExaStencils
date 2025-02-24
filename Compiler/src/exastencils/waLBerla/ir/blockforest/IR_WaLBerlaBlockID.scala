package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes._

case class IR_WaLBerlaBlockID(name : String, block : IR_WaLBerlaBlock) extends IR_WaLBerlaBlockLoopVariable {
  override def resolveName() : String = name
  override def resolveDatatype() : IR_Datatype = WB_BlockID

  override def getDeclaration() : IR_VariableDeclaration = IR_VariableDeclaration(resolveDatatype(), resolveName(), Some(block.getId()))

  def getTreeIndex() = IR_MemberFunctionCall(this, "getTreeIndex")
  def getTreeId() = IR_MemberFunctionCall(this, "getTreeId")
  def getFatherId() = IR_MemberFunctionCall(this, "getFatherId")
}
