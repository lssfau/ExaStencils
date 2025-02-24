package exastencils.waLBerla.ir.communication

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

case class IR_WaLBerlaCommunicationId(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_WaLBerlaInterfaceMember(true, false, false) {
  override def isPrivate : Boolean = true

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()
  private def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), fragmentIdx, IR_NullExpression, IR_NullExpression)

  override def name = s"wbCommId"
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}
