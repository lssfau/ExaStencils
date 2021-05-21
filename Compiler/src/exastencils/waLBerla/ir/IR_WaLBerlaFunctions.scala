package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.core.ObjectWithState
import exastencils.core.StateManager
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions

/// IR_WaLBerlaFunctions

object IR_WaLBerlaFunctions extends ObjectWithState {
  def defBase = "exa_waLBerla"
  def defBasePath = s"$defBase/$defBase"
  def defHeader = s"$defBasePath.h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_WaLBerlaFunctions] = None

  override def clear() = {
    selfRef = None
  }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_WaLBerlaFunctions]()
    selfRef.get
  }
}

case class IR_WaLBerlaFunctions() extends IR_FunctionCollection(IR_WaLBerlaFunctions.defBasePath,
  ListBuffer(), // external deps
  ListBuffer(IR_GlobalCollection.defHeader, IR_WaLBerlaSweep.defHeader)) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"

  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.cuda_enabled)
    internalDependencies += CUDA_KernelFunctions.defHeader

  if (Knowledge.opt_vectorize)
    if (Platform.simd_header != null) externalDependencies += Platform.simd_header

  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.startNode.isDefined) {
      super.printToFile()
      IR_WaLBerlaSweep(IR_WaLBerlaSweepGenerationContext(IR_WaLBerlaUtil.startNode.get)).printToFile()
    }
  }
}