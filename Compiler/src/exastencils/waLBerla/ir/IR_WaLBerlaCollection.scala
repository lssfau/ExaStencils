package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_VariableAccess
import exastencils.base.ir.IR_VariableDeclaration
import exastencils.baseExt.ir.IR_FunctionCollection
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.core.ObjectWithState
import exastencils.core.StateManager
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions

/// IR_WaLBerlaFunctions

object IR_WaLBerlaCollection extends ObjectWithState {
  def defBase = "exa_waLBerla"
  def defBasePath = s"$defBase/$defBase"
  def defHeader = s"$defBasePath.h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[IR_WaLBerlaCollection] = None

  override def clear() : Unit = {
    selfRef = None
  }

  // looks itself up starting from the current root
  def get : IR_WaLBerlaCollection = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[IR_WaLBerlaCollection]()
    selfRef.get
  }
}

case class IR_WaLBerlaCollection(var variables : ListBuffer[IR_VariableDeclaration] = ListBuffer()) extends IR_FunctionCollection(IR_WaLBerlaCollection.defBasePath,
  ListBuffer(), // external deps
  ListBuffer(IR_GlobalCollection.defHeader)) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"

  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"

  if (Knowledge.cuda_enabled)
    internalDependencies += CUDA_KernelFunctions.defHeader

  if (Knowledge.opt_vectorize)
    if (Platform.simd_header != null) externalDependencies += Platform.simd_header

  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.functorNodes.nonEmpty) {
      val contexts = IR_WaLBerlaUtil.functorNodes.map(IR_WaLBerlaFunctorGenerationContext)

      // append functor headers to internal deps
      contexts.foreach(context => { internalDependencies += IR_WaLBerlaFunctor.defHeader(context.className) })

      // print header for collection
      super.printToFile()

      // print functors
      contexts.foreach(context => { IR_WaLBerlaFunctor(context).printToFile() })
    }
  }
}

object IR_WaLBerlaReplaceVariableAccesses extends DefaultStrategy("Find and append suffix") {
  this += Transformation("Replace", {
    case acc : IR_VariableAccess =>
      val isWaLBerlaVar = IR_WaLBerlaCollection.get.variables.contains(IR_VariableDeclaration(acc))
      val isFunctorParam = IR_WaLBerlaUtil.functorNodes.exists(f => f.parameters.exists(p => p.name == acc.name && p.datatype == acc.datatype))

      if ( isWaLBerlaVar || isFunctorParam )
        IR_VariableAccess(IR_WaLBerlaUtil.getMemberName(acc.name), acc.datatype)
      else
        acc
  })
}