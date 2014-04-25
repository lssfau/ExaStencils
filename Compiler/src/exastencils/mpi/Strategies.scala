package exastencils.mpi

import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._

object RemoveMPIReferences extends Strategy("RemoveMPIReferences") {
  this += new Transformation("CleaningFunctions", {
    // FIXME: should delete node, currently not fully implemented -> QUICKFIX returns empty statements
    case _ : MPI_Barrier        => new NullStatement
    case _ : MPI_Finalize       => new NullStatement
    case _ : MPI_Init           => new NullStatement
    case _ : MPI_SetRankAndSize => new NullStatement

    case _ : MPI_IsRootProc     => BooleanConstant(true)
  })
}