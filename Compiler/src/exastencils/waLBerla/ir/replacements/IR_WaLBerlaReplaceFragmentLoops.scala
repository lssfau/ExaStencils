package exastencils.waLBerla.ir.replacements

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks

object IR_WaLBerlaReplaceFragmentLoops extends IR_WaLBerlaReplacementStrategy("Replace fragment loops over waLBerla fields") {

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments =>
      if (containsWaLBerlaFieldAccesses(loopOverFrags))
        IR_WaLBerlaLoopOverLocalBlocks(loopOverFrags.body, loopOverFrags.parallelization)
      else
        loopOverFrags
  })
}
