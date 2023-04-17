package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

case class IR_WaLBerlaLocalBlocks() extends IR_WaLBerlaInterfaceMember(false, false, false) {

  override def name : String = "localBlocks"

  override def resolveMemberBaseAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), datatype)
  private def resolveAccess() = resolveMemberBaseAccess()

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("std::vector< Block* >")

  override def isPrivate : Boolean = true

  // blocks
  private def getBlocks() = IR_MemberFunctionCallArrow(IR_WaLBerlaBlockForest(), "getBlocks", resolveAccess(), /* level */ 0) // TODO: adjust when refinement is supported

  override def getCtor() : Option[IR_Statement] = Some(getBlocks())
}
