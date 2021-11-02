package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._
import exastencils.waLBerla.ir.IR_WaLBerlaUtil.getBlockForest

/// IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaLoopOverBlocks {
  def defIt = IR_VariableAccess("block", IR_SpecialDatatype("auto"))
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed in loop
    var fieldsAccessed = ListBuffer[IR_WaLBerlaField]()
    IR_CollectAccessedWaLBerlaFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_CollectAccessedWaLBerlaFields.wbFieldAccesses).groupBy(_.name).map(_._2.head)

    import IR_WaLBerlaLoopOverBlocks._

    // TODO for multiple waLBerla blocks and exa fragments: association between them

    new IR_ForLoop(
      IR_VariableDeclaration(defIt, IR_MemberFunctionCallArrow(getBlockForest, "begin", defIt.datatype)),
      IR_Neq(defIt, IR_MemberFunctionCallArrow(getBlockForest, "end", defIt.datatype)),
      IR_ExpressionStatement(IR_PreIncrement(defIt)),
      IR_WaLBerlaUtil.getFields(fieldsAccessed : _*) ++ body,
      parallelization)
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial()
  })
}
