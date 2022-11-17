package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._
import exastencils.waLBerla.ir.field._

/// IR_WaLBerlaLoopOverBlocks

object IR_WaLBerlaLoopOverBlocks {
  def defIt = IR_WaLBerlaBlockForest().iterator
}

// iterates through process-local blocks
case class IR_WaLBerlaLoopOverBlocks(
    var body : ListBuffer[IR_Statement],
    var parallelization : IR_ParallelizationInfo = IR_ParallelizationInfo()) extends IR_ScopedStatement with IR_SpecialExpandable with IR_HasParallelizationInfo {

  // waLBerla block loops are not gpu-parallelizable atm
  parallelization.gpuParallelizable = false

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed per level in loop
    var fieldsAccessed = ListBuffer[IR_WaLBerlaFieldAccess]()
    IR_WaLBerlaCollectAccessedFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_WaLBerlaCollectAccessedFields.wbFieldAccesses).groupBy(_.name).flatMap(_._2.groupBy(_.level).map(_._2.head))

    import IR_WaLBerlaLoopOverBlocks._

    def getWaLBerlaFieldData(accesses : IR_WaLBerlaFieldAccess*) : ListBuffer[IR_Statement] = {
      accesses.to[mutable.ListBuffer].flatMap(fAcc => {
        ListBuffer(
          IR_IV_WaLBerlaGetFieldData(fAcc).getDeclaration(),
          IR_IfCondition(Knowledge.waLBerla_useInternalMemoryPointers,
            IR_IV_WaLBerlaFieldDataAt(fAcc).getDeclaration()))
      })
    }

    val blockForest = IR_WaLBerlaBlockForest()

    // TODO for multiple waLBerla blocks and exa fragments: association between them

    new IR_ForLoop(
      IR_VariableDeclaration(defIt, blockForest.begin()),
      IR_Neq(defIt, blockForest.end()),
      IR_PreIncrement(defIt),
      getWaLBerlaFieldData(fieldsAccessed : _*) ++ body,
      parallelization)
  }
}

object IR_WaLBerlaResolveLoopOverBlocks extends DefaultStrategy("Resolve waLBerla LoopOverBlocks") {
  this += Transformation("Resolve", {
    case loop : IR_WaLBerlaLoopOverBlocks => loop.expandSpecial()
  })
}
