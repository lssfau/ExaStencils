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
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.util.ir.IR_StackCollector

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

  override def printHeader() = {
    val writer = PrettyprintingManager.getPrinter(s"$baseName.h")
    for (inc <- internalDependencies)
      writer.addInternalDependency(inc)
    for (inc <- externalDependencies)
      writer.addExternalDependency(inc)

    writer <<< "namespace walberla {\nnamespace exastencils { "

    // header only functions
    for (func <- functions)
      if (func.isHeaderOnly)
        writer <<< func.prettyprint

    // functions with separate definition
    for (func <- functions)
      if (!func.isHeaderOnly && !func.hasAnnotation("deviceOnly"))
        writer << func.prettyprint_decl

    writer <<< "}\n}"
  }

  override def printSources() = {
    // will be overwritten for kernel functions
    for (func <- functions)
      if (!func.isHeaderOnly) {
        val writer = PrettyprintingManager.getPrinter(s"${ baseName }_${ func.name }.cpp")
        writer.addInternalDependency(s"$baseName.h")

        writer <<< "namespace walberla {\nnamespace exastencils { "

        writer <<< func.prettyprint

        writer <<< "}\n}"
      }
  }

  override def printToFile() : Unit = {
    if (IR_WaLBerlaUtil.waLBerlafunctionNodes.nonEmpty) {
      // append interface header to internal dependencies
      internalDependencies += IR_WaLBerlaInterface.interfaceHeader

      // print header for collection
      super.printToFile()
    }
  }
}

object IR_WaLBerlaReplaceVariableAccesses extends DefaultStrategy("Find and append suffix") {

  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += Transformation("Replace", {
    case acc : IR_VariableAccess =>
      val isWaLBerlaVar = IR_WaLBerlaCollection.get.variables.contains(IR_VariableDeclaration(acc))
      val isWaLBerlaFuncionParam = {
        val enclosingWbFunc = collector.stack.collectFirst { case e : IR_WaLBerlaFunction => e }
        if (enclosingWbFunc.isDefined)
          enclosingWbFunc.get.parameters.exists(p => p.name == acc.name && p.datatype == acc.datatype)
        else
          false
      }

      if ( isWaLBerlaVar || isWaLBerlaFuncionParam )
        IR_VariableAccess(IR_WaLBerlaUtil.getGeneratedName(acc.name), acc.datatype)
      else
        acc
  })
}