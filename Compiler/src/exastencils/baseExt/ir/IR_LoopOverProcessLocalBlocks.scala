package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.parallelization.ir.IR_ParallelizationInfo

trait IR_LoopOverProcessLocalBlocks extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {
  def body : ListBuffer[IR_Statement]
  def parallelization : IR_ParallelizationInfo

  final def defIt = IR_LoopOverFragments.defIt
}
