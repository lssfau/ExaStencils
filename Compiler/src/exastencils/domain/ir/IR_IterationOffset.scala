package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.prettyprinting.PpStream

/// IR_IV_IterationOffsetBegin

case class IR_IV_IterationOffsetBegin(var dim : Int, var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"iterationOffsetBegin_${ IR_DimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(1)
  def resolveAccess = super.resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)
}

/// IR_IV_IterationOffsetEnd

case class IR_IV_IterationOffsetEnd(var dim : Int, var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"iterationOffsetEnd_${ IR_DimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype() = IR_IntegerDatatype
  override def resolveDefValue() = Some(-1)
}
