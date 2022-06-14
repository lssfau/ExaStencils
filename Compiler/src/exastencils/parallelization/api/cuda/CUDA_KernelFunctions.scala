//=============================================================================
//
//  This file is part of the ExaStencils code generation framework. ExaStencils
//  is free software: you can redistribute it and/or modify it under the terms
//  of the GNU General Public License as published by the Free Software
//  Foundation, either version 3 of the License, or (at your option) any later
//  version.
//
//  ExaStencils is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with ExaStencils. If not, see <http://www.gnu.org/licenses/>.
//
//=============================================================================

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
  ListBuffer("cmath", "algorithm", "limits"), // provide math functions like sin, etc. as well as commonly used functions like min/max by default
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
  var generatedRedKernels = mutable.HashSet[String]()
  var requiredRedKernels = mutable.HashSet[(String, IR_Expression, CUDA_Stream)]()
  var counterMap = mutable.HashMap[String, Int]()

  def getRedKernelName(op : String, dt : IR_Datatype) =
    "DefaultReductionKernel" + IR_BinaryOperators.opAsIdent(op) + (if (dt.isInstanceOf[IR_MatrixDatatype]) dt.prettyprint else "")

  def getRedKernelWrapperName(op : String, dt : IR_Datatype) =
    getRedKernelName(op, dt) + "_wrapper"

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
    for ((op, target, stream) <- requiredRedKernels) addDefaultReductionKernel(op, target, stream)
    requiredRedKernels.clear // consume reduction requests
  }

  override def printSources() = {
    for (f <- functions if !f.isHeaderOnly) {
      var fileName = f.asInstanceOf[IR_Function].name
      if (fileName.endsWith(CUDA_Kernel.wrapperPostfix)) fileName = fileName.dropRight(CUDA_Kernel.wrapperPostfix.length)
      val writer = PrettyprintingManager.getPrinter(s"${ baseName }_$fileName.cu")
      writer.addInternalDependency(s"$baseName.h")

      writer <<< f.prettyprint(PrintEnvironment.CUDA)
      writer <<< ""
    }
  }

  def addDefaultReductionKernel(op : String, target : IR_Expression, stream : CUDA_Stream) : Unit = {
    val reductionDt = CUDA_Util.getReductionDatatype(target)
    val kernelName = getRedKernelName(op, reductionDt)
    val wrapperName = getRedKernelWrapperName(op, reductionDt)

    // early exit if already generated
    if (generatedRedKernels.contains(kernelName))
      return
    else
      generatedRedKernels += kernelName

    // kernel function
    {
      def data = IR_FunctionArgument("data", IR_PointerDatatype(reductionDt.resolveBaseDatatype))
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

      val assign : IR_Statement = reductionDt match {
        case mat : IR_MatrixDatatype =>
            // reduction of whole matrix
            val i = IR_VariableAccess("_i", IR_IntegerDatatype)
            val j = IR_VariableAccess("_j", IR_IntegerDatatype)
            val matSize = mat.sizeN * mat.sizeM
            val matIdx = i * mat.sizeN + j
            val dst = IR_ArrayAccess(data.access, matSize * it + matIdx)
            val src = IR_ArrayAccess(data.access, matSize * it + matSize * halfStride.access + matIdx)
            IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
              IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
                IR_Assignment(dst, IR_BinaryOperators.createExpression(op, dst, src))))))
        case _ =>
          // matrix element or scalar quantity
          val dst = IR_ArrayAccess(data.access, it)
          val src = IR_ArrayAccess(data.access, it + halfStride.access)
          IR_Assignment(dst, IR_BinaryOperators.createExpression(op, dst, src))
      }

      // add values with stride
      fctBody += IR_IfCondition(
        IR_Lower(it + halfStride.access, numElements.access),
        assign)

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

      def data = IR_FunctionArgument("data", IR_PointerDatatype(reductionDt.resolveBaseDatatype))
      var functionArgs = ListBuffer(data, numElements)
      if (Knowledge.domain_numFragmentsPerBlock > 1)
        functionArgs += IR_FunctionArgument(IR_LoopOverFragments.defIt)

      def blockSize = Knowledge.cuda_reductionBlockSize

      var fctBody = ListBuffer[IR_Statement]()

      // compile args
      def blocks = IR_VariableAccess("blocks", IR_SpecialDatatype("size_t"))
      val execCfg = CUDA_ExecutionConfiguration(Array[IR_Expression](blocks), Array[IR_Expression](blockSize), stream)
      val kernelCallArgs = ListBuffer[IR_Expression](data.access, numElements.access, halfStride)

      // compile loop body
      var loopBody = ListBuffer[IR_Statement]()
      loopBody += IR_VariableDeclaration(blocks, (numElements.access + (blockSize * 2 * halfStride - 1)) / (blockSize * 2 * halfStride))
      //loopBody += IR_IfCondition(IR_EqEq(0, blocks), IR_Assignment(blocks, 1)) // blocks cannot become 0 if numElements is positive
      loopBody += CUDA_FunctionCall(kernelName, kernelCallArgs, execCfg)

      fctBody += IR_ForLoop(
        IR_VariableDeclaration(halfStride, 1),
        IR_Lower(halfStride, numElements.access),
        IR_Assignment(halfStride, 2, "*="),
        loopBody)

      // call default reduction kernel and return by copying to passed (host) pointer
      // TODO: temporary solution until the reductions are optimized
      val matrixReductionTmp = IR_FunctionArgument("matrixReductionTmp", data.datatype)
      functionArgs += matrixReductionTmp
      if (Knowledge.cuda_useManagedMemory) {
        // D-D copy to reduction buffer
        fctBody += CUDA_Memcpy(matrixReductionTmp.access, data.access, IR_SizeOf(reductionDt), "cudaMemcpyDeviceToDevice")
      }
      fctBody += CUDA_TransferUtil.genTransfer(matrixReductionTmp.access, data.access, IR_SizeOf(reductionDt), "D2H", stream)

      // compile final wrapper function
      val fct = IR_PlainFunction(/* FIXME: IR_LeveledFunction? */
        wrapperName,
        IR_UnitDatatype,
        functionArgs,
        fctBody)

      fct.allowInlining = false
      fct.allowFortranInterface = false
      fct.functionQualifiers = "extern \"C\""

      functions += fct
    }
  }
}
