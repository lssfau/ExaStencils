package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.baseExt.ir.IR_LoopOverProcessLocalBlocks
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation.Output
import exastencils.parallelization.ir.IR_ParallelizationInfo

/// IR_WaLBerlaLoopOverLocalBlockArray

object IR_WaLBerlaLoopOverLocalBlockArray {
  def apply(body : IR_Statement*) = new IR_WaLBerlaLoopOverLocalBlockArray(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_WaLBerlaLoopOverLocalBlockArray(ListBuffer(body), parallelization)

  def defIt = IR_LoopOverFragments.defIt
  def block = IR_WaLBerlaBlock()
}

case class IR_WaLBerlaLoopOverLocalBlockArray(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()
) extends IR_LoopOverProcessLocalBlocks {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // array of process-local blocks
    val blockArray = IR_WaLBerlaLocalBlocks()

    val upperBoundKnown = Knowledge.waLBerla_useGridPartFromExa
    val upperBound : IR_Expression = if (upperBoundKnown)
      Knowledge.domain_numFragmentsPerBlock
    else
      IR_Cast(IR_IntegerDatatype, blockArray.size())

    /* assemble loop that facilitates access to wb data structures */
    var compiledBody = ListBuffer[IR_Statement]()

    // check if there are block loop variables to be added (i.e. declared and set)
    IR_WaLBerlaFindBlockLoopVariables.applyStandalone(IR_Scope(body))
    for (blockVar <- IR_WaLBerlaFindBlockLoopVariables.blockLoopVariables.distinct.sorted)
      compiledBody += blockVar.getDeclaration()

    compiledBody ++= body

    val loop = IR_ForLoop(
      IR_VariableDeclaration(defIt, 0),
      IR_Lower(defIt, upperBound),
      IR_PreIncrement(defIt),
      compiledBody,
      parallelization)

    if (upperBoundKnown)
      loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)

    loop
  }
}