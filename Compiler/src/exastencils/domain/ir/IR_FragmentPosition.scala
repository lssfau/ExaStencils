package exastencils.domain.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.deprecated.ir.IR_DimToString
import exastencils.polyhedron.IR_PolyScalarAccessLike
import exastencils.prettyprinting.PpStream

/// IR_IV_FragmentPosition

case class IR_IV_FragmentPosition(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPos_${ IR_DimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)
}

/// IR_IV_FragmentPositionBegin

case class IR_IV_FragmentPositionBegin(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPosBegin_${ IR_DimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)

  override def uniqueID : String = resolveName()
}

/// IR_IV_FragmentPositionEnd

case class IR_IV_FragmentPositionEnd(dim : Int, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_InternalVariable(true, false, false, false, false) with IR_Access with IR_PolyScalarAccessLike {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, IR_NullExpression, IR_NullExpression, IR_NullExpression)

  override def resolveName() = s"fragmentPosEnd_${ IR_DimToString(dim) }" + resolvePostfix(fragmentIdx.prettyprint, "", "", "", "")
  override def resolveDatatype() = IR_RealDatatype
  override def resolveDefValue() = Some(0.0)

  override def uniqueID : String = resolveName()
}
