package exastencils.waLBerla.ir.interfacing

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config._
import exastencils.core._
import exastencils.datastructures._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.parallelization.api.cuda.CUDA_KernelFunctions
import exastencils.prettyprinting.PrettyprintingManager
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.util.IR_WaLBerlaUtil

/// IR_WaLBerlaCollection

object IR_WaLBerlaCollection extends ObjectWithState {
  def defBase = "exa_coupling"
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

  if (Knowledge.mpi_enabled) {
    addExternalDependency("mpi.h")
    addExternalDependency("core/mpi/MPIManager.h")
  }

  if (Knowledge.cuda_enabled)
    addExternalDependency("cuda/GPUField.h")
  else if (Platform.targetHardware == "CPU")
    addExternalDependency("field/GhostLayerField.h")
  addExternalDependency("core/DataTypes.h")
  addExternalDependency("stencil/all.h")
  addExternalDependency("blockforest/communication/UniformBufferedScheme.h")
  addExternalDependency("field/SwapableCompare.h")
  addExternalDependency("core/cell/Cell.h")
  addExternalDependency("field/communication/PackInfo.h")
  addExternalDependency("domain_decomposition/BlockDataID.h")
  addExternalDependency("domain_decomposition/IBlock.h")
  addExternalDependency("domain_decomposition/StructuredBlockStorage.h")
  addExternalDependency("set")

  if (Knowledge.omp_enabled)
    addExternalDependency("omp.h")

  if (Knowledge.cuda_enabled)
    internalDependencies += CUDA_KernelFunctions.defHeader

  if (Knowledge.opt_vectorize)
    if (Platform.simd_header != null) addExternalDependency(Platform.simd_header)

  var interfaceContext : Option[IR_WaLBerlaInterfaceGenerationContext] = None
  var interfaceInstance : Option[IR_WaLBerlaInterface] = None

  // add future functions
  functions ++= IR_WaLBerlaInitWrapperFunctions.functions
  functions ++= IR_WaLBerlaInitExaWrapperFunctions.functions
  functions ++= IR_WaLBerlaDeInitWrapperFunctions.functions
  functions ++= IR_WaLBerlaDeInitExaWrapperFunctions.functions
  functions ++= IR_WaLBerlaInitCouplingWrapperFunctions.functions
  functions ++= IR_WaLBerlaHelperFunctionCollection.functions
  functions ++= IR_WaLBerlaGetterFunctionCollection.functions

  // collect future function names
  val futureFunctionIds : ListBuffer[String] = Duplicate(functions).collect { case f : IR_WaLBerlaFutureFunction => f }.map(_.name)

  def addExternalDependency(dependency : String) : Unit = {
    if (!externalDependencies.contains(dependency))
      externalDependencies += dependency
  }

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

    // handle inlined implementations
    for (func <- functions.filter(_.isInstanceOf[IR_WaLBerlaFunction])) {
      if (func.asInstanceOf[IR_WaLBerlaFunction].inlineImplementation && !func.isHeaderOnly) {
        val source = s"${ baseName }_${ func.name }.impl.h"
        val writerImpl = PrettyprintingManager.getPrinter(source)

        writer << "#include \"" + source + "\"\n"

        writerImpl <<< "namespace walberla {\nnamespace exastencils { "
        writerImpl <<< func.prettyprint
        writerImpl <<< "}\n}"
      }
    }
  }

  override def printSources() = {
    // filter out already handled inline implementations
    val separatedFuncImpls = functions.filter {
      case f : IR_WaLBerlaFunction => !f.inlineImplementation
      case _                       => true
    }

    for (func <- separatedFuncImpls)
      if (!func.isHeaderOnly) {
        val writer = PrettyprintingManager.getPrinter(s"${ baseName }_${ func.name }.cpp")
        writer.addInternalDependency(IR_WaLBerlaInterface.interfaceHeader)

        writer <<< "namespace walberla {\nnamespace exastencils { "

        writer <<< func.prettyprint

        writer <<< "}\n}"
      }
  }

  override def printToFile() : Unit = {
    val foundUserFunction = functions exists {
      case f : IR_LeveledFunctionLike => !futureFunctionIds.contains(f.baseName)
      case f : IR_PlainFunctionLike => !futureFunctionIds.contains(f.name)
    }

    if (foundUserFunction || interfaceInstance.isDefined)
      super.printToFile()
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