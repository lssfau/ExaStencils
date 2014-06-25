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
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiRank")
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiSize")
      }
      globals
    case func : FunctionStatement if (("initGlobals" : Expression) == func.name) =>
      if (Knowledge.useMPI) {
        func.body += "mpiCommunicator = " + Knowledge.mpi_defaultCommunicator
        func.body += "MPI_Comm_rank(mpiCommunicator, &mpiRank)"
        func.body += "MPI_Comm_size(mpiCommunicator, &mpiSize)"
      }
      func
  })
}
