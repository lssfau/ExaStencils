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

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir.IR_ScopedStatement
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatOperations.IR_GenerateBasicMatrixOperations
import exastencils.baseExt.ir._
import exastencils.config.Knowledge
import exastencils.core._
import exastencils.datastructures._
import exastencils.logger.Logger
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.solver.ir.IR_InlineMatSolveStmts
import exastencils.util.ir.IR_CommunicationKernelCollector
import exastencils.util.ir.IR_FctNameCollector
import exastencils.util.ir.IR_FragmentLoopCollector
import exastencils.util.ir.IR_StackCollector

/// CUDA_ExtractHostAndDeviceCode

/**
  * This transformation is used to convert annotated code into CUDA kernel code.
  */
object CUDA_ExtractHostAndDeviceCode extends DefaultStrategy("Transform annotated CUDA loop in kernel code") {
  val fctNameCollector = new IR_FctNameCollector
  val stackCollector = new IR_StackCollector
  val fragLoopCollector = new IR_FragmentLoopCollector
  val commKernelCollector = new IR_CommunicationKernelCollector
  this.register(fctNameCollector)
  this.register(stackCollector)
  this.register(fragLoopCollector)
  this.register(commKernelCollector)
  this.onBefore = () => this.resetCollectors()

  /**
    * Collect all loops in the band.
    *
    * @param loop the outer loop of the band
    * @return list of loops in the band
    */
  def collectLoopsInKernel(loop : IR_ForLoop, condition : IR_ForLoop => Boolean) : ListBuffer[IR_ForLoop] = {
    val innerLoopCandidate = loop.body.head
    val loops = ListBuffer[IR_ForLoop](loop)

    innerLoopCandidate match {
      case innerLoop : IR_ForLoop if condition.apply(innerLoop) =>
        collectLoopsInKernel(innerLoop, condition) ++ loops
      case _                                                    =>
        loops
    }
  }

  /**
    * Trim the loop headers to have the inner loop body to spare.
    *
    * @param body    the statements that should be traversed
    * @param surplus the for loop statements that should be removed
    * @return the inner body without loop headers
    */
  @tailrec
  def pruneKernelBody(body : ListBuffer[IR_Statement], surplus : ListBuffer[IR_ForLoop]) : ListBuffer[IR_Statement] = {
    if (surplus.isEmpty || !(body.head.isInstanceOf[IR_ForLoop] && (body.head equals surplus.head))) {
      body
    } else {
      pruneKernelBody(body.head.asInstanceOf[IR_ForLoop].body, surplus.tail)
    }
  }

  this += new Transformation("Processing ForLoopStatement nodes", {
    case loop : IR_ForLoop if loop.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) &&
      loop.getAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION).contains(CUDA_Util.CUDA_BAND_START) =>

      // remove the annotation first to guarantee single application of this transformation.
      loop.removeAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION)

      val parallelLoops = (x : IR_ForLoop) => {
        x.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) &&
          x.getAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION).contains(CUDA_Util.CUDA_BAND_PART)
      }
      val allLoops = (x : IR_ForLoop) => {
        x.hasAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION) &&
          (x.getAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION).contains(CUDA_Util.CUDA_BAND_PART) ||
            x.getAnnotation(CUDA_Util.CUDA_LOOP_ANNOTATION).contains(CUDA_Util.CUDA_INNER))
      }
      val parallelInnerLoops = collectLoopsInKernel(loop, parallelLoops)
      val allInnerLoops = collectLoopsInKernel(loop, allLoops)
      val (loopVariables, lowerBounds, upperBounds, stepSize) = CUDA_Util.extractRelevantLoopInformation(allInnerLoops)
      val kernelBody = pruneKernelBody(ListBuffer[IR_Statement](loop), parallelInnerLoops.reverse)

      if (loop.hasAnnotation(CUDA_Util.CUDA_BODY_DECL))
        loop.getAnnotation(CUDA_Util.CUDA_BODY_DECL).getOrElse(ListBuffer[IR_VariableDeclaration]()).asInstanceOf[ListBuffer[IR_VariableDeclaration]].foreach(x => kernelBody.prepend(x))

      val deviceStatements = ListBuffer[IR_Statement]()

      // add kernel and kernel call
      val kernelFunctions = CUDA_KernelFunctions.get

      val kernelCount = kernelFunctions.counterMap.getOrElse(fctNameCollector.getCurrentName, -1) + 1

      val reduction = Duplicate(loop.parallelization.reduction)
      val redTarget = if (reduction.isDefined)
        Some(Duplicate(reduction.get.target))
      else
        None

      // local variable for kernels with reductions
      val localTarget = if (reduction.isDefined)
        Some(IR_VariableAccess(reduction.get.targetName + "_local_" + kernelCount, CUDA_Util.getReductionDatatype(redTarget.get)))
      else
        None

      // collect local accesses because their variables need to be passed to the kernel when calling
      CUDA_GatherVariableAccesses.clear()
      CUDA_GatherVariableAccesses.fctName = fctNameCollector.getCurrentName
      CUDA_GatherVariableAccesses.kernelCount = kernelCount
      if (reduction.isDefined)
        CUDA_GatherVariableAccesses.reductionTarget = redTarget
      CUDA_GatherVariableAccesses.applyStandalone(IR_Scope(loop))

      // declare and init local reduction target
      if (localTarget.isDefined) {
        var decl = IR_VariableDeclaration(localTarget.get)
        var initLocalTarget = CUDA_Util.getReductionDatatype(redTarget.get) match {
          case _ : IR_ScalarDatatype   =>
            ListBuffer[IR_Statement](IR_Assignment(localTarget.get, redTarget.get))
          case mat : IR_MatrixDatatype =>
            redTarget.get match {
              case vAcc : IR_VariableAccess =>
                IR_GenerateBasicMatrixOperations.loopSetSubmatrixMatPointer(
                  vAcc, localTarget.get, mat.sizeN, mat.sizeM, mat.sizeN, 0, 0).body
              case expr                     =>
                Logger.error("Cannot set submatrix for expression: " + expr)
            }
        }

        // also detect accesses coming from the init of the local target
        CUDA_GatherVariableAccesses.applyStandalone(IR_Scope(decl))
        CUDA_GatherVariableAccesses.applyStandalone(IR_Scope(initLocalTarget))

        // replace array accesses with accesses to function arguments
        CUDA_ReplaceNonReductionVarArrayAccesses.reductionTarget = None // actually allow reduction var to be replaced here
        CUDA_ReplaceNonReductionVarArrayAccesses.applyStandalone(IR_Scope(decl))
        CUDA_ReplaceNonReductionVarArrayAccesses.applyStandalone(IR_Scope(initLocalTarget))

        kernelBody.prepend(initLocalTarget : _*)
        kernelBody.prepend(decl)
      }

      // access collections
      val accesses = CUDA_GatherVariableAccesses.evaluableAccesses.toSeq.sortBy(_._1).to[ListBuffer]
      val accessesCopiedToDevice = CUDA_GatherVariableAccesses.nonEvaluableAccesses.toSeq.sortBy(_._1).to[ListBuffer]

      // add non-evaluable accesses in form of pointers to device copies
      val deviceArrayCopies = accessesCopiedToDevice.map {
        case (k, v) =>
          val copyName = CUDA_GatherVariableAccesses.arrayVariableAccessAsString(v._1)
          val copyBaseDt = v._2.resolveBaseDatatype
          val size = v._2.getSizeArray.product

          (k, CUDA_MatrixDeviceCopy(copyName, copyBaseDt, size))
      }.toMap

      // parameters of the kernel
      val params = ListBuffer[IR_FunctionArgument]()
      params ++= accesses.map { case (name, tup) => IR_FunctionArgument(name, tup._2) }
      params ++= deviceArrayCopies.values.map(cpy => cpy.asFuncArg())

      // args passed to kernel
      val args = ListBuffer[IR_Expression]()
      args ++= accesses.map { case (_, tup) => tup._1 : IR_Expression }
      args ++= deviceArrayCopies.values

      var extremaMap = mutable.HashMap[String, (Long, Long)]()

      for (m <- loop.getAnnotation(IR_SimplifyExpression.EXTREMA_MAP))
        extremaMap = m.asInstanceOf[mutable.HashMap[String, (Long, Long)]]

      // inline contained calls to solve functions to avoid separate compilation units
      var scope = IR_Scope(kernelBody)
      IR_InlineMatSolveStmts.applyStandalone(scope)

      // replace array accesses with accesses to function arguments
      // reduction var is not replaced, but later in IR_HandleReductions
      if (reduction.isDefined)
        CUDA_ReplaceNonReductionVarArrayAccesses.reductionTarget = redTarget
      else
        CUDA_ReplaceNonReductionVarArrayAccesses.reductionTarget = None
      CUDA_ReplaceNonReductionVarArrayAccesses.applyStandalone(scope)

      // get enclosing frag loop
      val enclosingFragLoop = fragLoopCollector.getEnclosingFragmentLoop()

      // fetch stream from annotated inner/outer loop
      val annotOuterLoop = stackCollector.stack.collectFirst {
        case l : IR_ScopedStatement with IR_HasParallelizationInfo if l.hasAnnotation(CUDA_Util.CUDA_EXECUTION_STREAM) => l }
      val annotInnerLoop = collectLoopsInKernel(loop, (x : IR_ForLoop) => x.hasAnnotation(CUDA_Util.CUDA_EXECUTION_STREAM)).head
      val stream = if (annotOuterLoop.isDefined) {
        annotOuterLoop.get.popAnnotationAs[CUDA_Stream](CUDA_Util.CUDA_EXECUTION_STREAM)
      } else if (annotInnerLoop.hasAnnotation(CUDA_Util.CUDA_EXECUTION_STREAM)) {
        annotInnerLoop.popAnnotationAs[CUDA_Stream](CUDA_Util.CUDA_EXECUTION_STREAM)
      } else {
        Logger.error("Could not determine stream when extracting device code")
      }

      val kernel = CUDA_Kernel(
        kernelCount,
        kernelFunctions.getIdentifier(fctNameCollector.getCurrentName),
        parallelInnerLoops.length,
        params,
        Duplicate(loopVariables),
        Duplicate(lowerBounds),
        Duplicate(upperBounds),
        Duplicate(stepSize),
        Duplicate(scope.body),
        Duplicate(stream),
        Duplicate(reduction),
        Duplicate(localTarget),
        Duplicate(extremaMap))

      kernelFunctions.addKernel(Duplicate(kernel))

      // copy array variables from host to device if necessary
      // TODO: temporary solution until the reductions are optimized
      if (deviceArrayCopies.nonEmpty) {
        deviceArrayCopies foreach { case (k, dstArr) =>
          val (srcArr, srcDt) = accessesCopiedToDevice.find(_._1 == k).get._2
          if (Knowledge.cuda_useManagedMemory) {
            // H-H copy, possibly prefetch
            deviceStatements += IR_FunctionCall(IR_ExternalFunctionReference("std::copy", IR_UnitDatatype),
              ListBuffer[IR_Expression](srcArr, srcArr + srcDt.typicalByteSize, dstArr))
            deviceStatements += CUDA_TransferUtil.genTransfer(dstArr, dstArr, srcDt.typicalByteSize, "H2D", stream)
          } else {
            deviceStatements += CUDA_TransferUtil.genTransfer(srcArr, dstArr, srcDt.typicalByteSize, "H2D", stream)
          }
        }
      }

      // process return value of kernel wrapper call if reduction is required
      val callKernel = IR_FunctionCall(kernel.getWrapperFctName, args)
      if (reduction.isDefined) {
        // tmp buffer for reduction result (host). already set up in CUDA_HandleFragmentLoops
        val reductionTmp = if (enclosingFragLoop.isDefined) {
          val tmpBuf = enclosingFragLoop.get.popAnnotationAs[Option[CUDA_ReductionResultBuffer]](CUDA_Util.CUDA_REDUCTION_RESULT_BUF)
          if(tmpBuf.isEmpty)
            Logger.error("Temporary reduction result buffer has not been set up.")

          tmpBuf
        } else {
          None
        }

        // call kernel and pass allocated tmp buffer by pointer
        callKernel.arguments ++= reductionTmp
      }

      // kernel call
      deviceStatements += callKernel

      deviceStatements
  }, false)
}
