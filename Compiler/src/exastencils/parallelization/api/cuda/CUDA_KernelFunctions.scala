package exastencils.parallelization.api.cuda

import scala.collection.mutable
import scala.collection.mutable._

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.globals.ir.IR_GlobalCollection
import exastencils.prettyprinting._

/// CUDA_KernelFunctions

object CUDA_KernelFunctions extends ObjectWithState {
  def defBaseName = "Kernel/Kernel"
  def defHeader = defBaseName + ".h"

  // buffer looked up reference to reduce execution time
  var selfRef : Option[CUDA_KernelFunctions] = None

  override def clear() = { selfRef = None }

  // looks itself up starting from the current root
  def get = {
    if (selfRef.isEmpty)
      selfRef = StateManager.findFirst[CUDA_KernelFunctions]()
    selfRef.get
  }
}

case class CUDA_KernelFunctions() extends IR_FunctionCollection(CUDA_KernelFunctions.defBaseName,
  ListBuffer("cmath", "algorithm"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
  ListBuffer(IR_GlobalCollection.defHeader)) {

  externalDependencies += "iostream" // required for error messages

  if (Knowledge.mpi_enabled)
    externalDependencies += "mpi.h"
  if (Knowledge.omp_enabled)
    externalDependencies += "omp.h"
  if (Knowledge.cuda_enabled) {
    externalDependencies += "cuda.h"
    externalDependencies += "cuda_runtime.h"
  }

  var kernelCollection = ListBuffer[CUDA_Kernel]()
  var requiredRedKernels = mutable.HashSet[String]()
  var counterMap = mutable.HashMap[String, Int]()

  def getIdentifier(fctName : String) : String = {
    val cnt = counterMap.getOrElse(fctName, -1) + 1
    counterMap.update(fctName, cnt)
    s"${ fctName }_k${ String.format("%03d", cnt : java.lang.Integer) }"
  }

  def addKernel(kernel : CUDA_Kernel) = {
    kernelCollection += kernel
  }

  def convertToFunctions() = {
    for (kernel <- kernelCollection) {
      functions += kernel.compileKernelFunction
      functions += kernel.compileWrapperFunction
    }
    kernelCollection.clear // consume processed kernels

    // take care of reductions
    for (op <- requiredRedKernels) addDefaultReductionKernel(op)
    requiredRedKernels.clear // consume reduction requests
  }

  override def printSources() = {
    for (f <- functions) {
      var fileName = f.asInstanceOf[IR_Function].name
      if (fileName.endsWith(CUDA_Kernel.wrapperPostfix)) fileName = fileName.dropRight(CUDA_Kernel.wrapperPostfix.length)
      val writer = PrettyprintingManager.getPrinter(s"${ baseName }_$fileName.cu")
      writer.addInternalDependency(s"$baseName.h")

      writer <<< f.prettyprint(PrintEnvironment.CUDA)
      writer <<< ""
    }
  }

  def addDefaultReductionKernel(op : String) = {
    val opAsIdent = IR_BinaryOperators.opAsIdent(op)
    val kernelName = "DefaultReductionKernel" + opAsIdent
    val wrapperName = kernelName + "_wrapper"

    // kernel function
    {
      def data = IR_FunctionArgument("data", IR_PointerDatatype(IR_RealDatatype))
      def numElements = IR_FunctionArgument("numElements", IR_IntegerDatatype /*FIXME: size_t*/)
      def halfStride = IR_FunctionArgument("halfStride", IR_IntegerDatatype /*FIXME: size_t*/)
      def it = Duplicate(IR_LoopOverDimensions.defItForDim(0))

      var fctBody = ListBuffer[IR_Statement]()

      // add index calculation
      // FIXME: datatype for VariableAccess
      fctBody += IR_VariableDeclaration(it,
        IR_MemberAccess(IR_VariableAccess("blockIdx", IR_IntegerDatatype), "x") *
          IR_MemberAccess(IR_VariableAccess("blockDim", IR_IntegerDatatype), "x") +
          IR_MemberAccess(IR_VariableAccess("threadIdx", IR_IntegerDatatype), "x"))
      fctBody += IR_Assignment(it, 2 * halfStride.access, "*=")

      // add index bounds conditions
      fctBody += IR_IfCondition(
        IR_OrOr(IR_Lower(it, 0), IR_GreaterEqual(it, numElements.access)),
        IR_Return())

      // add values with stride
      fctBody += IR_IfCondition(
        IR_Lower(it + halfStride.access, numElements.access),
        IR_Assignment(IR_ArrayAccess(data.access, it), IR_BinaryOperators.createExpression(op, IR_ArrayAccess(data.access, it), IR_ArrayAccess(data.access, it + halfStride.access))))

      // compile final kernel function
      val fct = IR_PlainFunction(/* FIXME: IR_LeveledFunction? */ kernelName, IR_UnitDatatype, ListBuffer(data, numElements, halfStride), fctBody)

      fct.allowInlining = false
      fct.allowFortranInterface = false
      fct.functionQualifiers = "__global__"

      fct.annotate("deviceOnly")

      functions += fct
    }

    // wrapper function
    {
      def numElements = IR_FunctionArgument("numElements", IR_SpecialDatatype("size_t") /*FIXME*/)
      def halfStride = IR_VariableAccess("halfStride", IR_SpecialDatatype("size_t") /*FIXME*/)
      def data = IR_FunctionArgument("data", IR_PointerDatatype(IR_RealDatatype))
      def ret = IR_VariableAccess("ret", IR_RealDatatype)

      def blockSize = Knowledge.cuda_reductionBlockSize

      var fctBody = ListBuffer[IR_Statement]()

      // compile loop body
      def blocks = IR_VariableAccess("blocks", IR_SpecialDatatype("size_t"))
      var loopBody = ListBuffer[IR_Statement]()
      loopBody += IR_VariableDeclaration(blocks, (numElements.access + (blockSize * 2 * halfStride - 1)) / (blockSize * 2 * halfStride))
      //loopBody += IR_IfCondition(IR_EqEq(0, blocks), IR_Assignment(blocks, 1)) // blocks cannot become 0 if numElements is positive
      loopBody += CUDA_FunctionCall(kernelName, ListBuffer[IR_Expression](data.access, numElements.access, halfStride),
        Array[IR_Expression](blocks), Array[IR_Expression](blockSize))

      fctBody += IR_ForLoop(
        IR_VariableDeclaration(halfStride, 1),
        IR_Lower(halfStride, numElements.access),
        IR_Assignment(halfStride, 2, "*="),
        loopBody)

      fctBody += IR_VariableDeclaration(ret)
      fctBody += CUDA_Memcpy(IR_AddressOf(ret), data.access, IR_SizeOf(IR_RealDatatype), "cudaMemcpyDeviceToHost")

      fctBody += IR_Return(Some(ret))

      // compile final wrapper function
      val fct = IR_PlainFunction(/* FIXME: IR_LeveledFunction? */
        wrapperName,
        IR_RealDatatype, // TODO: support other types
        ListBuffer(data, IR_FunctionArgument("numElements", IR_IntegerDatatype /*FIXME: size_t*/)),
        fctBody)

      fct.allowInlining = false
      fct.allowFortranInterface = false
      fct.functionQualifiers = "extern \"C\""

      functions += fct
    }
  }
}
