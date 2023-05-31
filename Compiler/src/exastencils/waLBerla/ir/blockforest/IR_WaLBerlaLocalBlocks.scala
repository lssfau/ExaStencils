package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember
import exastencils.waLBerla.ir.util.IR_WaLBerlaDatatypes.WB_Block

case class IR_WaLBerlaLocalBlocks() extends IR_WaLBerlaInterfaceMember(false, false, false) {

  override def name : String = "localBlocks"

  override def resolveMemberBaseAccess() : IR_VariableAccess = IR_VariableAccess(resolveName(), datatype)
  private def resolveAccess() = resolveMemberBaseAccess()

  override def resolveDatatype() : IR_Datatype = IR_SpecialDatatype("std::vector< Block* >")

  override def isPrivate : Boolean = true

  override def getCtor() : Option[IR_Statement] = {
    val block = IR_VariableAccess("block", IR_SpecialDatatype("auto"))
    val blockforest = IR_WaLBerlaBlockForest()

    Some(
      IR_ForLoop(
        IR_VariableDeclaration(block, blockforest.begin()),
        IR_Neq(block, blockforest.end()),
        IR_PreIncrement(block),
        ListBuffer[IR_Statement](
          IR_MemberFunctionCall(resolveAccess(), "push_back",
            IR_DynamicCast(IR_PointerDatatype(WB_Block), IR_AddressOf(IR_DerefAccess(block)))))))
  }
}
