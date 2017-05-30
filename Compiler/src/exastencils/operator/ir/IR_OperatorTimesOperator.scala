package exastencils.operator.ir

import exastencils.base.ir.IR_Expression
import exastencils.prettyprinting.PpStream
import exastencils.util.ir.IR_ResultingDatatype

/// IR_OperatorTimesOperator

case class IR_OperatorTimesOperator(var lhs : IR_OperatorAccess, var rhs : IR_OperatorAccess) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(lhs.datatype, rhs.datatype)
  override def prettyprint(out : PpStream) = out << lhs << " * " << rhs
}
