package exastencils.waLBerla.ir.blockforest

import exastencils.base.ir._
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

case class IR_WaLBerlaLocalBlockIndicesFromRemote() extends IR_WaLBerlaInterfaceMember(false, false, false) {

  override def name : String = "localBlockIndicesFromRemote"

  override def resolveMemberBaseAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), datatype)
  private def resolveAccess() = resolveMemberBaseAccess()

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("std::map<BlockID, int>")

  override def isPrivate : Boolean = true

  override def getCtor() : Option[IR_Statement] = None
}
