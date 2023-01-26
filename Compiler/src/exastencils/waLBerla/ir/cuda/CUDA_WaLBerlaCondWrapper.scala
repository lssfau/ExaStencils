package exastencils.waLBerla.ir.cuda

import exastencils.base.ir._
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.config.Knowledge
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.util.NoDuplicateWrapper

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

object CUDA_WaLBerlaCondWrapper {
  def getNoDuplicateWrapper(stmt : IR_Statement, potentiallyParallel : Boolean) : NoDuplicateWrapper[IR_Expression] = {
    val condWrapper = NoDuplicateWrapper[IR_Expression](null)
    if (Knowledge.cuda_enabled && potentiallyParallel) {
      condWrapper.value = Knowledge.cuda_preferredExecution match {
        case "Host"        => // CPU by default
          IR_BooleanConstant(true)
        case "Device"      => // GPU by default
          IR_BooleanConstant(false)
        case "Performance" => // decide according to performance estimates
          FindLoopOverDimensions.applyStandalone(IR_Scope(stmt))

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
    condWrapper
  }
}