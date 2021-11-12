package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._

/// IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaLoopOverBlocks {
  def defIt = IR_WaLBerlaBlockForest().iterator
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed in loop
    var fieldsAccessed = ListBuffer[IR_WaLBerlaFieldAccess]()
    IR_WaLBerlaCollectAccessedFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_WaLBerlaCollectAccessedFields.wbFieldAccesses).groupBy(_.name).map(_._2.head)

    import IR_WaLBerlaLoopOverBlocks._

    val blockForest = IR_WaLBerlaBlockForest()

    // TODO for multiple waLBerla blocks and exa fragments: association between them

    new IR_ForLoop(
      IR_VariableDeclaration(defIt, blockForest.begin()),
      IR_Neq(defIt, blockForest.end()),
      IR_PreIncrement(defIt),
      defIt.getFields(fieldsAccessed : _*) ++ body,
      parallelization)
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial()
  })
}
