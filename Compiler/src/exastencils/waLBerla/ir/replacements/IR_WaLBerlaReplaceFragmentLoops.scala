package exastencils.waLBerla.ir.replacements

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.fieldlike.ir.IR_FieldLikeAccessLike
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.field._

object IR_WaLBerlaReplaceFragmentLoops extends IR_WaLBerlaReplacementStrategy("Replace fragment loops over waLBerla fields") {

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments =>
      if (containsWaLBerlaFieldAccesses(loopOverFrags))
        IR_WaLBerlaLoopOverBlocks(loopOverFrags.body, loopOverFrags.parallelization)
      else
        loopOverFrags
  })
}
