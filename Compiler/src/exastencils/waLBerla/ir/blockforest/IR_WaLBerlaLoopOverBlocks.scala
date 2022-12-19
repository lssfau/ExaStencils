package exastencils.waLBerla.ir.blockforest

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.datastructures.Transformation.Output
import exastencils.datastructures._
import exastencils.parallelization.ir._
import exastencils.util.NoDuplicateWrapper
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
  parallelization.gpuParallelizable = true

  def expandSpecial() : Output[IR_ForLoop] = {
    // TODO: separate omp and potentiallyParallel
    parallelization.potentiallyParallel = Knowledge.omp_enabled && Knowledge.omp_parallelizeLoopOverFragments && parallelization.potentiallyParallel

    // collect fields accessed per level in loop
    var fieldsAccessed = ListBuffer[IR_MultiDimWaLBerlaFieldAccess]()
    IR_WaLBerlaCollectAccessedFields.applyStandalone(body)
    fieldsAccessed ++= Duplicate(IR_WaLBerlaCollectAccessedFields.wbFieldAccesses).groupBy(_.name).flatMap(_._2.groupBy(_.level).map(_._2.head))

    import IR_WaLBerlaLoopOverBlocks._

    // TODO: separate CUDA handling into specialized strategy
    object FindLoopOverDimensions extends QuietDefaultStrategy("Find loop over dimensions") {
      var loopOverDims : Option[IR_LoopOverDimensions] = None

      override def applyStandalone(node : Node) : Unit = {
        loopOverDims = None
        super.applyStandalone(node)
      }

      this += Transformation("..", {
        case loopOverDimensions : IR_LoopOverDimensions =>
          loopOverDims = Some(loopOverDimensions)
          loopOverDimensions
      })
    }

    val condWrapper = NoDuplicateWrapper[IR_Expression](null)
    if (Knowledge.cuda_enabled) {
      condWrapper.value = Knowledge.cuda_preferredExecution match {
        case "Host"        => // CPU by default
          IR_BooleanConstant(true)
        case "Device"      => // GPU by default
          IR_BooleanConstant(false)
        case "Performance" => // decide according to performance estimates
          FindLoopOverDimensions.applyStandalone(IR_Scope(body))

          if (FindLoopOverDimensions.loopOverDims.isDefined) {
            val loop = FindLoopOverDimensions.loopOverDims.get
            IR_BooleanConstant(loop.getAnnotation("perf_timeEstimate_host").get.asInstanceOf[Double] <= loop.getAnnotation("perf_timeEstimate_device").get.asInstanceOf[Double])
          } else {
            IR_BooleanConstant(true)
          }
        case "Condition"   =>
          Knowledge.cuda_executionCondition
      }
    } else {
      condWrapper.value = IR_BooleanConstant(true) // CPU by default
    }

    def getWaLBerlaFieldData(accesses : IR_MultiDimWaLBerlaFieldAccess*) : ListBuffer[IR_Statement] = {
      accesses.to[mutable.ListBuffer].flatMap(fAcc => {
          IR_IV_WaLBerlaGetField(fAcc).getDeclarationBlockLoop(condWrapper) :+
          IR_IfCondition(Knowledge.waLBerla_useInternalMemoryPointers,
            IR_IV_WaLBerlaFieldData(fAcc).getDeclarationBlockLoop(condWrapper))
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
