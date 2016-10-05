package exastencils.mpi.ir

import exastencils.base.ir.IR_IntegerConstant
import exastencils.datastructures._

/// MPI_RemoveMPI

// TODO: prevent generation in the first place
object MPI_RemoveMPI extends DefaultStrategy("Remove references to mpi functions and variables") {
  this += new Transformation("Clean", {
    // TODO: think about replacing reduce, gather, etc. with copy operations
    case _ : MPI_Statement => List()
    case MPI_IV_MpiRank    => IR_IntegerConstant(0)
  })
}
