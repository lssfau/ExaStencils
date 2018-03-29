package exastencils.baseExt.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config._
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._
import exastencils.util.ir.IR_ReplaceVariableAccess

object IR_LoopOverFragments {
  def apply(body : IR_Statement*) = new IR_LoopOverFragments(body.to[ListBuffer])
  def apply(body : IR_Statement, parallelization : IR_ParallelizationInfo) = new IR_LoopOverFragments(ListBuffer(body), parallelization)

  def defIt = IR_VariableAccess("fragmentIdx", IR_IntegerDatatype)
}

case class IR_LoopOverFragments(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  import IR_LoopOverFragments.defIt

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    val loop = IR_ForLoop(
      IR_VariableDeclaration(defIt, 0),
      IR_Lower(defIt, Knowledge.domain_numFragmentsPerBlock),
      IR_PreIncrement(defIt),
      body,
      parallelization)
    loop.annotate("numLoopIterations", Knowledge.domain_numFragmentsPerBlock)
    loop
  }
}

/// IR_ResolveLoopOverFragments

object IR_ResolveLoopOverFragments extends DefaultStrategy("Resolve LoopOverFragments nodes") {
  this += new Transformation("Resolve", {
    case loop : IR_LoopOverFragments =>
      if (Knowledge.experimental_resolveUnreqFragmentLoops && Knowledge.domain_numFragmentsPerBlock <= 1) {
        // eliminate fragment loops in case of only one fragment per block
        val scope = IR_Scope(loop.body)

        // replace references to old loop iterator
        IR_ReplaceVariableAccess.replace = Map(IR_LoopOverFragments.defIt.name -> IR_IntegerConstant(0))
        IR_ReplaceVariableAccess.applyStandalone(scope)

        // check if scoping is necessary
        var scoping = false
        scope.body.foreach {
          case _ : IR_VariableDeclaration => scoping = true
          case _                          =>
        }

        if (scoping)
          scope
        else
          scope.body
      } else {
        // no elimination - simple expand
        loop.expandSpecial()
      }
  })
}
