package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.config._
import exastencils.core.ObjectWithState
import exastencils.core.StateManager
import exastencils.field.ir.IR_InitFieldsWithZero
import exastencils.simd.SIMD_NeonDivision

/// IR_UserFunctions

object IR_UserFunctions extends ObjectWithState {
  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_UserFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_UserFunctions]()
    selfRef.get
  }
}

case class IR_UserFunctions() extends IR_FunctionCollection("MultiGrid/MultiGrid",
  ListBuffer("cmath", "algorithm", "iostream"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "Util/TimerFunctions.h", "CommFunctions/CommFunctions.h", "Domains/DomainGenerated.h")) {

  // add conditional dependencies - TODO: move to respective packages
  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"

    internalDependencies += "KernelFunctions/KernelFunctions.h"
  }
  if (Knowledge.opt_vectorize) {
    val header = Platform.simd_header
    if (header != null)
      externalDependencies += header
    if (Platform.simd_instructionSet == "NEON")
      functions += SIMD_NeonDivision
    val mathLibHeader = Platform.simd_mathLibHeader
    if (mathLibHeader != null)
      externalDependencies ++= mathLibHeader
  }
  // TODO: move to fields package
  if (Knowledge.data_initAllFieldsWithZero)
    functions += IR_InitFieldsWithZero()

  if (!Knowledge.experimental_internalHighDimTypes)
    internalDependencies += "Util/Matrix.h"

  override def toString : String = "IR_UserFunctions(" + baseName + ", " + externalDependencies + ", " + internalDependencies + ", " + functions + ")"
}
