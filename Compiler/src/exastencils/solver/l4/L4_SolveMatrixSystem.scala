
package exastencils.solver.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.l4.L4_Expression
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.l4.L4_MatShape
import exastencils.prettyprinting.PpStream
import exastencils.solver.ir
import exastencils.solver.ir.IR_SolveMatrixSystem

object L4_SolveMatrixSystem {
  def apply(A : L4_Expression, u : L4_Expression, f : L4_Expression) = {

    new L4_SolveMatrixSystem(A, u, f)
  }
  def apply(A : L4_Expression, u : L4_Expression, f : L4_Expression, shape : Option[L4_MatShape]) = {

    new L4_SolveMatrixSystem(A, u, f, shape)
  }
}

//TODO support for matrix expressions
case class L4_SolveMatrixSystem(A : L4_Expression, u : L4_Expression, f : L4_Expression, shape : Option[L4_MatShape] = None) extends L4_Statement {
  override def progress : IR_SolveMatrixSystem = ProgressLocation {
    ir.IR_SolveMatrixSystem(A.progress, u.progress.asInstanceOf[IR_VariableAccess], f.progress.asInstanceOf[IR_VariableAccess],
      if(shape.isDefined) Some(shape.get.progress) else None
    )
  }
  override def prettyprint(out : PpStream) : Unit = {
    out << "solveMatSys " << A << ", " << u << ", " << f
    if(shape.isDefined) out << shape.get
  }
}