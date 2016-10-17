package exastencils.communication.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.field.ir.IR_Field
import exastencils.prettyprinting._

/// IR_IV_LocalCommReady

case class IR_IV_LocalCommReady(var field : IR_Field, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommReady" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_VolatileDatatype(IR_BooleanDatatype)
  override def resolveDefValue = Some(false)
}

/// IR_IV_LocalCommDone

case class IR_IV_LocalCommDone(var field : IR_Field, var neighIdx : IR_Expression, var fragmentIdx : IR_Expression = IR_LoopOverFragments.defIt) extends IR_IV_CommVariable {
  override def prettyprint(out : PpStream) : Unit = out << resolveAccess(resolveName, fragmentIdx, IR_NullExpression, field.index, field.level, neighIdx)

  override def resolveName = s"localCommDone" + resolvePostfix(fragmentIdx.prettyprint, "", field.index.toString, field.level.toString, neighIdx.prettyprint)
  override def resolveDatatype = IR_VolatileDatatype(IR_BooleanDatatype)
  override def resolveDefValue = Some(false)
}
