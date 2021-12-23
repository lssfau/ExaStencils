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
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.core._
import exastencils.datastructures._
import exastencils.optimization.ir.IR_SimplifyExpression
import exastencils.solver.ir.IR_InlineMatSolveStmts
import exastencils.util.ir.IR_FctNameCollector

/// CUDA_ExtractHostAndDeviceCode

/**
  * This transformation is used to convert annotated code into CUDA kernel code.
  */
object CUDA_ExtractHostAndDeviceCode extends DefaultStrategy("Transform annotated CUDA loop in kernel code") {
  val collector = new IR_FctNameCollector
  this.register(collector)
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
      loop.annotate(CUDA_Util.CUDA_LOOP_ANNOTATION)

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

      // collect local accesses because these variables need to be passed to the kernel at call
      CUDA_GatherVariableAccesses.clear()
      CUDA_GatherVariableAccesses.applyStandalone(IR_Scope(loop))
      val accesses = CUDA_GatherVariableAccesses.accesses.toSeq.sortBy(_._1).to[ListBuffer]

      var extremaMap = mutable.HashMap[String, (Long, Long)]()

      for (m <- loop.getAnnotation(IR_SimplifyExpression.EXTREMA_MAP))
        extremaMap = m.asInstanceOf[mutable.HashMap[String, (Long, Long)]]

      // inline contained calls to solve functions to avoid separate compilation units
      IR_InlineMatSolveStmts.applyStandalone(kernelBody)

      // replace array accesses with accesses to function arguments, ignore reduction variable
      if (loop.parallelization.reduction.isDefined)
        CUDA_ReplaceArrayAccesses.reductionVariable = Some(loop.parallelization.reduction.get.target)
      else
        CUDA_ReplaceArrayAccesses.reductionVariable = None
      CUDA_ReplaceArrayAccesses.applyStandalone(kernelBody)

      val kernel = CUDA_Kernel(
        kernelFunctions.getIdentifier(collector.getCurrentName),
        parallelInnerLoops.length,
        accesses.map { case (name, tup) => IR_FunctionArgument(name, tup._2) },
        Duplicate(loopVariables),
        Duplicate(lowerBounds),
        Duplicate(upperBounds),
        Duplicate(stepSize),
        Duplicate(kernelBody),
        Duplicate(loop.parallelization.reduction),
        Duplicate(extremaMap))

      kernelFunctions.addKernel(Duplicate(kernel))

      // process return value of kernel wrapper call if reduction is required
      val callKernel = IR_FunctionCall(kernel.getWrapperFctName, accesses.map { case (_, tup) => tup._1 : IR_Expression })
      if (loop.parallelization.reduction.isDefined) {
        val red = loop.parallelization.reduction.get
        CUDA_Util.getReductionDatatype(red.target) match {
          case mat : IR_MatrixDatatype =>
            val baseDt = mat.resolveBaseDatatype
            // declare and allocate tmp buffer for matrix reduction
            val reductionTmp = IR_VariableAccess("reductionTmpMatrix", IR_PointerDatatype(baseDt))
            deviceStatements += IR_VariableDeclaration(reductionTmp)
            deviceStatements += IR_ArrayAllocation(reductionTmp, baseDt, mat.sizeN * mat.sizeM)

            // call kernel and pass allocated tmp buffer by pointer
            callKernel.arguments += reductionTmp
            deviceStatements += callKernel

            // update reduction target
            val i = IR_VariableAccess("_i", IR_IntegerDatatype)
            val j = IR_VariableAccess("_j", IR_IntegerDatatype)
            val idx = i * mat.sizeN + j
            val dst = IR_ArrayAccess(red.target, idx)
            val src = IR_ArrayAccess(reductionTmp, idx)
            deviceStatements += IR_ForLoop(IR_VariableDeclaration(i, IR_IntegerConstant(0)), IR_Lower(i, mat.sizeM), IR_PreIncrement(i), ListBuffer[IR_Statement](
              IR_ForLoop(IR_VariableDeclaration(j, 0), IR_Lower(j, mat.sizeN), IR_PreIncrement(j), ListBuffer[IR_Statement](
                IR_Assignment(dst, IR_BinaryOperators.createExpression(red.op, dst, src))))))

            // free allocated buffer
            deviceStatements += IR_ArrayFree(reductionTmp)
          case _ : IR_ScalarDatatype   =>
            deviceStatements += IR_Assignment(red.target, IR_BinaryOperators.createExpression(red.op, red.target, callKernel))
        }
      } else {
        deviceStatements += callKernel
      }

      deviceStatements
  }, false)
}
