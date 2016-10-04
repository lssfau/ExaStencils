package exastencils.communication

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.knowledge._

case class CommunicationFunctions() extends IR_FunctionCollection("CommFunctions/CommFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/Vector.h", "Util/Matrix.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled)
    internalDependencies += "KernelFunctions/KernelFunctions.h"
  if (Knowledge.opt_vectorize) {
    val header = Platform.simd_header
    if (header != null) externalDependencies += header
  }
}

