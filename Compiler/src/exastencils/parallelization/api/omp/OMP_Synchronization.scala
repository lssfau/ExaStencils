package exastencils.parallelization.api.omp

import exastencils.base.ir.IR_Statement
import exastencils.prettyprinting.PpStream

/// OMP_Barrier

case object OMP_Barrier extends IR_Statement {
  exastencils.core.Duplicate.registerConstant(this)
  override def prettyprint(out : PpStream) : Unit = out << "#pragma omp barrier"
}
