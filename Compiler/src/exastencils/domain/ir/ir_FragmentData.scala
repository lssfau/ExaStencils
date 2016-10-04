package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.knowledge.dimToString
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentId

case class IR_IV_FragmentId(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"fragmentId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "size_t"
  override def resolveDefValue = Some(-1)
}

/// IR_IV_FragmentIndex

case class IR_IV_FragmentIndex(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"fragmentIndex_${ dimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(0)
}

/// IR_IV_IsValidForDomain

case class IR_IV_IsValidForDomain(var domain : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, true, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, domain, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"isValidForDomain" + resolvePostfix(fragmentIdx.prettyprint, domain.prettyprint, "", "", "")
  override def resolveDatatype = IR_BooleanDatatype
  override def resolveDefValue = Some(false)
}
