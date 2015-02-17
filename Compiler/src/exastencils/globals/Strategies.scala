package exastencils.globals

import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._

object AddDefaultGlobals extends DefaultStrategy("AddDefaultGlobals") {
  this += new Transformation("Adding default global constants and variables", {
    case globals: Globals =>
      if (Knowledge.mpi_enabled) {
        globals.variables += new VariableDeclarationStatement("MPI_Comm", "mpiCommunicator")
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiRank")
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "mpiSize")
      }
      if (!Knowledge.domain_rect_generate) {
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "numFragments")
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "fileOffset")
        globals.variables += new VariableDeclarationStatement(new IntegerDatatype, "bufsize")
      }
      globals
    case func: FunctionStatement if ("initGlobals" == func.name) =>
      if (Knowledge.mpi_enabled) {
        func.body += "mpiCommunicator = " + Knowledge.mpi_defaultCommunicator
        func.body += "MPI_Comm_rank(mpiCommunicator, &mpiRank)"
        func.body += "MPI_Comm_size(mpiCommunicator, &mpiSize)"
        func.body += "std::srand(mpiRank)"
      }
      if (!Knowledge.domain_rect_generate) {
        func.body += "numFragments = 0"
        func.body += "fileOffset = 0"
        func.body += "bufsize = 0"
      }
      func
  })
}
