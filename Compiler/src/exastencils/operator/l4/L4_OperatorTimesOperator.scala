package exastencils.operator.l4

import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_Expression
import exastencils.operator.ir.IR_OperatorTimesOperator
import exastencils.prettyprinting.PpStream

/// L4_OperatorTimesOperator

case class L4_OperatorTimesOperator(var left : L4_OperatorAccess, var right : L4_OperatorAccess) extends L4_Expression {
  def prettyprint(out : PpStream) = out << left << " * " << right
  override def progress = ProgressLocation(IR_OperatorTimesOperator(left.progress, right.progress))
}
