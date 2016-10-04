package exastencils.mpi.ir

import exastencils.base.ir._
import exastencils.baseExt.ir._

case class IR_IV_MpiRank() extends IR_UnduplicatedVariable {
  override def resolveName = "mpiRank"
  override def resolveDatatype = IR_IntegerDatatype
  
  // default value is not applicable since mpi rank will be initialized in a separate routine
  override def resolveDefValue = None
}
