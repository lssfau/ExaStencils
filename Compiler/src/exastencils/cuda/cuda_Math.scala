package exastencils.cuda

import exastencils.base.ir._
import exastencils.prettyprinting._
import exastencils.util.ir._

/// CUDA_MinimumExpression

case class CUDA_MinimumExpression(left : IR_Expression, right : IR_Expression) extends IR_Expression {
  override def datatype = IR_ResultingDatatype(left.datatype, right.datatype)
  override def prettyprint(out : PpStream) : Unit = out << "min(" << left << "," << right << ")"
}
