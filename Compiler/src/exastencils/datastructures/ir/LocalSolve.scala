package exastencils.datastructures.ir

import exastencils.base.ir._
import exastencils.prettyprinting._

// FIXME: update with actual accessors
case class hackVecComponentAccess(var vec : IR_VariableAccess, var i : IR_Expression) extends IR_Expression {
  override def datatype = vec.innerDatatype.getOrElse(IR_RealDatatype)
  override def prettyprint(out : PpStream) : Unit = out << vec << "(" << i << ", " << 0 << ")"
}

// FIXME: update with actual accessors
case class hackMatComponentAccess(var mat : IR_VariableAccess, var i : IR_Expression, var j : IR_Expression) extends IR_Expression {
  override def datatype = mat.innerDatatype.getOrElse(IR_RealDatatype)
  override def prettyprint(out : PpStream) : Unit = out << mat << "(" << i << ", " << j << ")"
}
