package exastencils.datastructures.ir.iv

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.prettyprinting._

/// general fragment parameters

/// geometric mapping

case class CommId(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"commId" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = IR_IntegerDatatype
  override def resolveDefValue = Some(-1)
}

case class PrimitiveTransformation(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"trafoMatrix" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Mat4"
  override def resolveDefValue = Some("Mat4()")
}

/// neighborhood information

