package exastencils.parallelization.api.mpi

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection

object MPI_AddGlobals extends NoTraversalStrategy("Extend globals for MPI") {
  override def doWork() : Unit = {
    val globals = IR_GlobalCollection.get

    // FIXME: introduce iv's
    globals.variables += IR_VariableDeclaration("MPI_Comm", "mpiCommunicator")
    globals.variables += IR_VariableDeclaration(IR_IntegerDatatype, "mpiSize")

    val initFunc = globals.functions.find(_.name == "initGlobals").get.asInstanceOf[IR_Function]

    initFunc.body += "mpiCommunicator = " + Knowledge.mpi_defaultCommunicator
    initFunc.body += "MPI_Comm_rank(mpiCommunicator, &mpiRank)"
    initFunc.body += "MPI_Comm_size(mpiCommunicator, &mpiSize)"
    initFunc.body += "std::srand(mpiRank)"
  }
}
