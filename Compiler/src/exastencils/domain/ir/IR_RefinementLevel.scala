package exastencils.domain.ir

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_InternalVariable
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.prettyprinting.PpStream

case class IR_IV_RefinementLevel(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def resolveName() : String = "refinementLevel" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() : IR_Datatype = IR_IntegerDatatype

  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveDefValue() : Option[IR_Expression] = Some(0)
}
