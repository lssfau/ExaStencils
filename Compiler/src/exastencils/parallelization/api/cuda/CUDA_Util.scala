package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
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
    loop.inc.isInstanceOf[IR_Assignment] &&
      loop.inc.asInstanceOf[IR_Assignment].src.isInstanceOf[IR_IntegerConstant] &&
      loop.isInstanceOf[IR_HasParallelizationInfo] && loop.asInstanceOf[IR_HasParallelizationInfo].parallelization.potentiallyParallel
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
}
