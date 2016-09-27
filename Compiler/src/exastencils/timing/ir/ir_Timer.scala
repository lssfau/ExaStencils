package exastencils.timing.ir

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_UnduplicatedVariable
import exastencils.logger.Logger

/// IR_IV_Timer

object IR_IV_Timer {
  def apply(name : IR_Expression) = new IR_IV_Timer(name match {
    case IR_StringConstant(value) => value
    case IR_StringLiteral(value)  => value
    case otherExpression          =>
      Logger.warn("Timer name is not constant: " + otherExpression)
      otherExpression.prettyprint()
  })
}

case class IR_IV_Timer(var name : String) extends IR_UnduplicatedVariable with IR_Access {
  override def resolveName = s"timer_" + stripName
  override def resolveDatatype = IR_SpecialDatatype("StopWatch")

  def stripName = name.replaceAll("[^a-zA-Z0-9]", "_")

  override def getCtor() : Option[IR_Statement] = {
    Some(IR_Assignment(
      IR_MemberAccess(IR_VariableAccess(resolveName, resolveDatatype), "timerName"),
      IR_StringConstant(stripName)))
  }
}
