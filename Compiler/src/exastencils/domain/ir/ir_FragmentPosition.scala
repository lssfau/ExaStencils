package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.knowledge.dimToString
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentPosition

// TODO: convert IR_IV_FragmentPosition to Real datatype
object IR_IV_FragmentPosition {
  def apply(dim : Int) = IR_ArrayAccess(new IR_IV_FragmentPosition(), dim)
  def apply(dim : Int, fragmentIdx : IR_Expression) = IR_ArrayAccess(new IR_IV_FragmentPosition(fragmentIdx), dim)
}

case class IR_IV_FragmentPosition(var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"pos" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = "Vec3"
  override def resolveDefValue = Some("Vec3(0, 0, 0)")
}

/// IR_IV_FragmentPositionBegin

case class IR_IV_FragmentPositionBegin(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"posBegin_${ dimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = IR_RealDatatype
  override def resolveDefValue = Some(0.0)
}

/// IR_IV_FragmentPositionEnd

case class IR_IV_FragmentPositionEnd(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName = s"posEnd_${ dimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype = IR_RealDatatype
  override def resolveDefValue = Some(0.0)
}
