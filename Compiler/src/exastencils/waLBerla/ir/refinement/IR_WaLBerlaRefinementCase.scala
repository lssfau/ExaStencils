package exastencils.waLBerla.ir.refinement

import exastencils.base.ir._
import exastencils.prettyprinting.PpStream
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

case class IR_WaLBerlaRefinementCase(
    var fragmentIdx : IR_Expression,
    var neighIdx : IR_Expression) extends IR_WaLBerlaInterfaceMember(true, false, true) {

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess()

  private def resolveAccess() : IR_Expression = resolveAccess(resolveMemberBaseAccess(), fragmentIdx, IR_NullExpression, neighIdx)

  override def resolveDefValue() : Option[IR_Expression] = Some(IR_IntegerConstant(-1))

  override def name : String = "wbRefinementCase"
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype
  override def isPrivate : Boolean = true
}
