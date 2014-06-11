package exastencils.globals

import exastencils.core._
import exastencils.knowledge._
import exastencils.datastructures._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.globals._
import exastencils.strategies._

object AddDefaultGlobals extends DefaultStrategy("AddDefaultGlobals") {
  this += new Transformation("Adding default global constants and variables", {
    case globals : Globals =>
      if (Knowledge.useMPI) {
        globals.variables += new VariableDeclarationStatement("MPI_Comm", "mpiCommunicator")
        globals.initFunction.body += "mpiCommunicator = " + Knowledge.mpi_defaultCommunicator

        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiRank")
        globals.initFunction.body += "MPI_Comm_rank(mpiCommunicator, &mpiRank)"
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiSize")
        globals.initFunction.body += "MPI_Comm_size(mpiCommunicator, &mpiSize)"
      }
      globals
  })
}
