package exastencils.cuda

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import exastencils.core._
import exastencils.data._
import exastencils.datastructures._
import exastencils.datastructures.Transformation._
import exastencils.datastructures.ir._
import exastencils.datastructures.ir.ImplicitConversions._
import exastencils.knowledge._
import exastencils.logger._
import exastencils.prettyprinting._

case class KernelFunctions() extends FunctionCollection("KernelFunctions/KernelFunctions",
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer("Globals/Globals.h", "MultiGrid/MultiGrid.h")) {

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.experimental_cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }

  var kernelCollection = ListBuffer[Kernel]()
  var counter = 1000

  def getIdentifier : String = {
    counter += 1
    s"kernel_$counter"
  }

  def addKernel(kernel : Kernel) = {
    kernelCollection += kernel
  }

  def convertToFunctions = {
    for (kernel <- kernelCollection) {
      functions += kernel.compileKernelFunction
      functions += kernel.compileWrapperFunction
    }
    kernelCollection.clear // consume processed kernels
  }

  override def printSources = {
    for (f <- functions) {
      var fileName = f.asInstanceOf[FunctionStatement].name
      if (fileName.endsWith(Kernel.wrapperPostfix)) fileName = fileName.dropRight(Kernel.wrapperPostfix.length)
      val writer = PrettyprintingManager.getPrinter(s"${baseName}_${fileName}.cu")
      writer.addInternalDependency(s"${baseName}.h")

      writer <<< f.prettyprint
      writer <<< ""
    }
  }
}

object Kernel {
  def wrapperPostfix = "_wrapper"
}

case class Kernel(var identifier : String,
    var passThroughArgs : ListBuffer[VariableAccess],
    var numDimensions : Int,
    var indices : IndexRange,
    var body : ListBuffer[Statement],
    var reduction : Option[Reduction] = None,
    var condition : Option[Expression] = None) extends Node {

  import Kernel._

  var evaluatedFieldAccesses = false
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()

  def getKernelFctName : String = identifier
  def getWrapperFctName : String = identifier + wrapperPostfix

  def evalFieldAccesses = {
    if (!evaluatedFieldAccesses) {
      GatherLocalLinearizedFieldAccess.fieldAccesses.clear
      GatherLocalLinearizedFieldAccess.applyStandalone(Scope(body))
      fieldAccesses = GatherLocalLinearizedFieldAccess.fieldAccesses
      evaluatedFieldAccesses = true
    }
  }

  def compileKernelBody : ListBuffer[Statement] = {
    evalFieldAccesses // ensure that field accesses have been mapped

    if (reduction.isDefined) Logger.warn("Kernels with reductions are currently not supported -> reduction will be ignored")

    var statements = ListBuffer[Statement]()

    // add index calculation
    statements ++= (0 until numDimensions).map(dim => {
      def it = dimToString(dim)
      VariableDeclarationStatement(IntegerDatatype, it,
        Some(("blockIdx." ~ it) * ("blockDim." ~ it) + ("threadIdx." ~ it)))
    })

    // add index bounds conditions
    statements ++= (0 until numDimensions).map(dim => {
      def it = Duplicate(LoopOverDimensions.defIt(dim))
      new ConditionStatement(
        OrOrExpression(LowerExpression(it, s"begin_$dim"), GreaterEqualExpression(it, s"end_$dim")),
        ReturnStatement())
    })

    // add other conditions if required
    if (condition.isDefined)
      statements += new ConditionStatement(NegationExpression(condition.get), ReturnStatement())

    // add actual body after replacing field accesses
    ReplacingLocalLinearizedFieldAccess.fieldAccesses = fieldAccesses
    ReplacingLocalLinearizedFieldAccess.applyStandalone(Scope(body))
    statements ++= body

    statements
  }

  def compileWrapperFunction : FunctionStatement = {
    evalFieldAccesses // ensure that field accesses have been mapped

    // compile arguments for device function call
    var callArgs = ListBuffer[Expression]()
    for (dim <- 0 until numDimensions) {
      callArgs += indices.begin(dim)
      callArgs += indices.end(dim)
    }
    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      callArgs += iv.FieldData(fieldSelection.field, fieldSelection.level, fieldSelection.slot)
    }
    for (variableAccess <- passThroughArgs) {
      callArgs += Duplicate(variableAccess)
    }

    FunctionStatement(
      SpecialDatatype("extern \"C\" void"), // FIXME
      getWrapperFctName,
      Duplicate(passThroughArgs),
      ListBuffer[Statement](CUDA_FunctionCallExpression(getKernelFctName, callArgs)),
      false)
  }

  def compileKernelFunction : FunctionStatement = {
    evalFieldAccesses // ensure that field accesses have been mapped

    // compile parameters for device function
    var fctParams = ListBuffer[VariableAccess]()
    for (dim <- 0 until numDimensions) {
      fctParams += VariableAccess(s"begin_$dim", Some(IntegerDatatype))
      fctParams += VariableAccess(s"end_$dim", Some(IntegerDatatype))
    }
    for (fieldAccess <- fieldAccesses) {
      val fieldSelection = fieldAccess._2.fieldSelection
      fctParams += VariableAccess(fieldAccess._1, Some(PointerDatatype(fieldSelection.field.dataType.resolveUnderlyingDatatype)))
    }
    for (variableAccess <- passThroughArgs) {
      fctParams += Duplicate(variableAccess)
    }

    var fct = FunctionStatement(
      SpecialDatatype("__global__ void"), // FIXME
      getKernelFctName,
      fctParams,
      compileKernelBody,
      false, false)

    fct.annotate("deviceOnly")

    fct
  }
}

object GatherLocalLinearizedFieldAccess extends QuietDefaultStrategy("Gathering local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()

  def mapFieldAccess(access : LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _                     => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    fieldAccesses.put(identifier, access)
  }

  this += new Transformation("Searching", {
    case access : LinearizedFieldAccess =>
      mapFieldAccess(access)
      access
  }, false)
}

object ReplacingLocalLinearizedFieldAccess extends QuietDefaultStrategy("Replacing local LinearizedFieldAccess nodes") {
  var fieldAccesses = HashMap[String, LinearizedFieldAccess]()

  def extractIdentifier(access : LinearizedFieldAccess) = {
    val field = access.fieldSelection.field
    var identifier = field.identifier

    // TODO: array fields
    if (field.numSlots > 1) {
      access.fieldSelection.slot match {
        case SlotAccess(_, offset) => identifier += s"_o$offset"
        case IntegerConstant(slot) => identifier += s"_s$slot"
        case _                     => identifier += s"_s${access.fieldSelection.slot.prettyprint}"
      }
    }

    identifier
  }

  this += new Transformation("Searching", {
    case access : LinearizedFieldAccess => {
      val identifier = extractIdentifier(access)
      val fieldAccess = fieldAccesses.get(identifier).get
      ArrayAccess(identifier, fieldAccess.index)
    }
  })
}

//__global__ void
//gpu_smoother(int begin0, int end0, int begin1, int end1, int begin2, int end2,
//  double *rhs, double *sol0, double *sol1, int3 iterationOffsetBegin, int3 iterationOffsetEnd) {
//
//  int x = blockIdx.x * blockDim.x + threadIdx.x;
//  int y = blockIdx.y * blockDim.y + threadIdx.y;
//  int z = blockIdx.z * blockDim.z + threadIdx.z;
//
//  if (x < iterationOffsetBegin.x || x >= iterationOffsetEnd.x + 257
//    || y < iterationOffsetBegin.y || y >= iterationOffsetEnd.y + 257
//    || z < iterationOffsetBegin.z || z >= iterationOffsetEnd.z + 257)
//    return;
//
//  sol1[((((z * 67081) + (y * 259)) + x) + 67341)] = ((sol0[((((z * 67081) + (y * 259)) + x) + 67341)] + (((((((rhs[(((z * 66049) + (y * 257)) + x)] + sol0[((((z * 67081) + (y * 259)) + x) + 134422)]) + sol0[((((z * 67081) + (y * 259)) + x) + 260)]) + sol0[((((z * 67081) + (y * 259)) + x) + 67082)]) + sol0[((((z * 67081) + (y * 259)) + x) + 67340)]) + sol0[((((z * 67081) + (y * 259)) + x) + 67342)]) + sol0[((((z * 67081) + (y * 259)) + x) + 67600)])*0.13333333333333333)) - (sol0[((((z * 67081) + (y * 259)) + x) + 67341)] * 0.8));
//}

//extern "C" void smootherWrapper()
//{
//  if (hostFieldUpdated...[]) { /* cuda copy */ }
//  gpu_smoother<<<dim3(36, 36, 36), dim3(8, 8, 8)>>>(
//        iterationOffsetBegin.x, iterationOffsetEnd.x, iterationOffsetBegin.y, iterationOffsetEnd.y, iterationOffsetBegin.z, iterationOffsetEnd.z,
//        gpu_rhs, gpu_sol[(currentSlot_Solution[6] + 0) % 2], gpu_sol[(currentSlot_Solution[6] + 1) % 2]);
//}
