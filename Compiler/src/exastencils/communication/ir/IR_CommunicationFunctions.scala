package exastencils.communication.ir

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config._
import exastencils.core.StateManager

/// IR_CommunicationFunctions

object IR_CommunicationFunctions {
  // looks itself up starting from the current root
  def get = StateManager.findFirst[IR_CommunicationFunctions]().get
}

case class IR_CommunicationFunctions() extends IR_FunctionCollection("CommFunctions/CommFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Matrix.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"

  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.cuda_enabled)
    internalDependencies += "KernelFunctions/KernelFunctions.h"

  if (Knowledge.opt_vectorize)
    if (Platform.simd_header != null) externalDependencies += Platform.simd_header
}
