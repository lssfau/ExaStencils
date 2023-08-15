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

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixDatatype
import exastencils.config.Knowledge
import exastencils.logger.Logger
import exastencils.parallelization.ir.IR_HasParallelizationInfo

/// CUDA_Util

/**
  * Collection of constants and util functions for the CUDA transformations.
  */
object CUDA_Util {
  val CUDA_LOOP_ANNOTATION = "CUDALoop"
  val CUDA_BAND_START = "CUDABandStart"
  val CUDA_BAND_PART = "CUDABandPart"
  val CUDA_INNER = "CUDAInner"
  val CUDA_BODY_DECL = "CUDABodyDeclarations"
  val CUDA_BRANCH_CONDITION = "CUDABranchCond"
  val CUDA_EXECUTION_STREAM = "CUDAExecutionStream"
  val CUDA_REDUCTION_RESULT_BUF = "CUDAReductionResultBuf"

  /**
    * Check if the loop meets some basic conditions for transforming a ForLoopStatement into CUDA code.
    *
    * @param loop the ForLoopStatement that should be checked
    * @return <code>true</code> if the loop meets the conditions; <code>false</code> otherwise
    */
  def verifyCudaLoopSuitability(loop : IR_ForLoop) : Boolean = {
    loop.begin.isInstanceOf[IR_VariableDeclaration] &&
      (loop.end.isInstanceOf[IR_Lower] || loop.end.isInstanceOf[IR_LowerEqual]) &&
      loop.inc.isInstanceOf[IR_Assignment]
  }

  /**
    * Check if the loop can be parallelized.
    *
    * @param loop the ForLoopStatement that should be checked
    * @return <code>true</code> if it is a parallel loop; <code>false</code> otherwise
    */
  def verifyCudaLoopParallel(loop : IR_ForLoop) : Boolean = {
    loop.inc.isInstanceOf[IR_Assignment] && loop.inc.asInstanceOf[IR_Assignment].src.isInstanceOf[IR_IntegerConstant] && (
      if (Knowledge.experimental_cuda_generateKernelForNonParallel) true
      else loop.isInstanceOf[IR_HasParallelizationInfo]
        && loop.asInstanceOf[IR_HasParallelizationInfo].parallelization.potentiallyParallel
        && loop.asInstanceOf[IR_HasParallelizationInfo].parallelization.gpuParallelizable )
  }

  /**
    * Collect information about the loop variables, lower and upper bounds, and the step size.
    *
    * @param loops the list of ForLoopStatement that should be traversed
    * @return lists of extracted information
    */
  def extractRelevantLoopInformation(loops : ListBuffer[IR_ForLoop]) = {
    var loopVariables = ListBuffer[String]()
    var lowerBounds = ListBuffer[IR_Expression]()
    var upperBounds = ListBuffer[IR_Expression]()
    var stepSize = ListBuffer[IR_Expression]()

    loops foreach { loop =>
      val loopDeclaration = loop.begin.asInstanceOf[IR_VariableDeclaration]
      loopVariables += loopDeclaration.name
      lowerBounds += loopDeclaration.initialValue.get
      upperBounds += (loop.end match {
        case l : IR_Lower      =>
          l.right
        case e : IR_LowerEqual =>
          IR_Addition(e.right, IR_IntegerConstant(1))
        case o                 => o
      })
      stepSize += (loop.inc match {
        case IR_Assignment(_, src : IR_IntegerConstant, "+=")                                               => src
        case IR_Assignment(it1, IR_Addition(ListBuffer(it2, step : IR_IntegerConstant)), "=") if it1 == it2 => step
        case IR_Assignment(it1, IR_Addition(ListBuffer(step : IR_IntegerConstant, it2)), "=") if it1 == it2 => step
      })
    }

    (loopVariables, lowerBounds, upperBounds, stepSize)
  }

  // get actual datatype of reduction target
  def getReductionDatatype(target : IR_Expression) = target.datatype match {
    case mat : IR_MatrixDatatype =>
      target match {
        case _ : IR_VariableAccess =>
          // whole matrix
          mat
        case _ : IR_ArrayAccess =>
          // matrix element
          mat.resolveBaseDatatype
      }
    case dt : IR_ScalarDatatype =>
      dt
    case dt : IR_Datatype =>
      Logger.error("Unsupported reduction datatype: " + dt.prettyprint() + ". Target = " + target.prettyprint())
  }

  // checks if args is the reduction target
  def isReductionTarget(target : Option[IR_Expression], expr : IR_Expression) = target.isDefined && target.get == expr

  // checks if arg is the reduction target itself or an indexed access to it
  def isReductionVariableAccess(target : Option[IR_Expression], arrAcc : IR_ArrayAccess) = {
    arrAcc.base match {
      case vAcc : IR_VariableAccess => isReductionTarget(target, vAcc) || isReductionTarget(target, arrAcc)
      case _                        => false
    }
  }

  def dimToMember(i : Int) : String = {
    i match {
      case 0 => "x"
      case 1 => "y"
      case 2 => "z"
      case _ => Logger.error(s"Invalid index for dimToMember: $i")
    }
  }
}
