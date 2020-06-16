import exastencils.base.ProgressLocation
import exastencils.base.l4.L4_PlainVariableAccess
import exastencils.base.l4.L4_Statement
import exastencils.prettyprinting.PpStream
import exastencils.solver.ir
import exastencils.solver.ir.IR_SolveLinearSystem

object L4_SolveLinearSystem {
  def apply(A : L4_PlainVariableAccess, u : L4_PlainVariableAccess, f : L4_PlainVariableAccess) = {
    new L4_SolveLinearSystem(A,u,f)
  }
}

//TODO relax
//TODO add jacobi type?
//TODO support for matrix expressions
case class L4_SolveLinearSystem(A : L4_PlainVariableAccess, u : L4_PlainVariableAccess, f : L4_PlainVariableAccess) extends L4_Statement {
  override def progress : IR_SolveLinearSystem = ProgressLocation {
    ir.IR_SolveLinearSystem(A.progress, u.progress, f.progress)
  }
  override def prettyprint(out : PpStream) : Unit = out << "solveLinearSystem(" + A.prettyprint(out) + ", " + f.prettyprint(out) + ")"
}