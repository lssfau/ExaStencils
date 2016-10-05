package exastencils.mpi.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._

/// IR_IV_MpiRank

case object MPI_IV_MpiRank extends IR_UnduplicatedVariable {
  exastencils.core.Duplicate.registerImmutable(this.getClass)

  override def resolveName = "mpiRank"
  override def resolveDatatype = IR_IntegerDatatype

  // default value is not applicable since mpi rank will be initialized in a separate routine
  override def resolveDefValue = None
}

/// MPI_IsRootProc

object MPI_IsRootProc {
  def apply() = 0 EqEq MPI_IV_MpiRank
}
