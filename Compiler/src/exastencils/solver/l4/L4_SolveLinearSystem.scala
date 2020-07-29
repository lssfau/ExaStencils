
package exastencils.solver.l4

import exastencils.base.ProgressLocation
import exastencils.base.ir.IR_VariableAccess
import exastencils.base.l4.L4_Expression
import exastencils.base.l4.L4_Statement
import exastencils.prettyprinting.PpStream
import exastencils.solver.ir
import exastencils.solver.ir.IR_SolveLinearSystem

object L4_SolveLinearSystem {
  def apply(A : L4_Expression, u : L4_Expression, f : L4_Expression) = {

    new L4_SolveLinearSystem(A, u, f)
  }
}

//TODO support for matrix expressions
case class L4_SolveLinearSystem(A : L4_Expression, u : L4_Expression, f : L4_Expression) extends L4_Statement {
  override def progress : IR_SolveLinearSystem = ProgressLocation {
    ir.IR_SolveLinearSystem(A.progress, u.progress.asInstanceOf[IR_VariableAccess], f.progress.asInstanceOf[IR_VariableAccess])
  }
  override def prettyprint(out : PpStream) : Unit = out << "solveLES " << A << ", " << u << ", " << f
}