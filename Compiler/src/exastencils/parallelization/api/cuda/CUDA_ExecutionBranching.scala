package exastencils.parallelization.api.cuda

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.config.Platform
import exastencils.util.NoDuplicateWrapper

// compile switch for cpu/gpu exec
trait CUDA_ExecutionBranching {
  def getHostDeviceBranchingMPI(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement]) : ListBuffer[IR_Statement] = {
    val defaultChoice : IR_Expression = Knowledge.cuda_preferredExecution match {
      case _ if !Platform.hw_gpu_gpuDirectAvailable => 1 // if GPUDirect is not available default to CPU
      case "Host"                                   => 1 // CPU by default
      case "Device"                                 => 0 // GPU by default
      case "Performance"                            => 1 // FIXME: Knowledge flag
      case "Condition"                              => Knowledge.cuda_executionCondition
    }

    ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
  }

  def getHostDeviceBranching(hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement], estimatedFasterHostExec : Boolean) : ListBuffer[IR_Statement] = {
    // get execution choice
    val defaultChoice = getDefaultChoice(estimatedFasterHostExec)

    ListBuffer[IR_Statement](IR_IfCondition(defaultChoice, hostStmts, deviceStmts))
  }

  private def getDefaultChoice(estimatedFasterHostExec : Boolean) : IR_Expression = {
    Knowledge.cuda_preferredExecution match {
      case "Host"        => IR_BooleanConstant(true) // CPU by default
      case "Device"      => IR_BooleanConstant(false) // GPU by default
      case "Performance" => IR_BooleanConstant(estimatedFasterHostExec) // decide according to performance estimates
      case "Condition"   => Knowledge.cuda_executionCondition
    }
  }

  def getHostDeviceBranchingCondWrapper(condWrapper : NoDuplicateWrapper[IR_Expression],
      hostStmts : ListBuffer[IR_Statement], deviceStmts : ListBuffer[IR_Statement], estimatedFasterHostExec : Boolean) : ListBuffer[IR_Statement] = {

    // get execution choice
    condWrapper.value = getDefaultChoice(estimatedFasterHostExec)

    // set dummy first to prevent IR_GeneralSimplify from removing the branch statement until the condition is final
    val branch = IR_IfCondition(IR_VariableAccess("replaceIn_CUDA_AnnotateLoops", IR_BooleanDatatype), hostStmts, deviceStmts)
    branch.annotate(CUDA_Util.CUDA_BRANCH_CONDITION, condWrapper)
    ListBuffer[IR_Statement](branch)
  }
}
