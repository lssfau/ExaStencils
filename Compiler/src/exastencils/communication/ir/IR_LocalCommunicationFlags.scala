package exastencils.communication.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.field.ir.IR_Field
import exastencils.prettyprinting._

/// IR_IV_RemoteReqOutstanding

case class IR_IV_RemoteReqOutstanding(var field : IR_Field, var direction : String, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName(), fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName() = s"remoteReqOutstanding_${ direction }" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype() = IR_BooleanDatatype
  override def resolveDefValue() = Some(false)
}
