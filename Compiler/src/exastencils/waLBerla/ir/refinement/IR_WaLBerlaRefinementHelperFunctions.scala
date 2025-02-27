package exastencils.waLBerla.ir.refinement

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_FunctionLike
import exastencils.config.Knowledge

object IR_WaLBerlaRefinementHelperFunctions {
  var functions : ListBuffer[IR_FunctionLike] = ListBuffer()

  functions += IR_WaLBerlaInitNonuniformBlockForest()
  functions += IR_WaLBerlaWorkloadAndMemoryAssignment()
  if (Knowledge.dimensionality == 2)
    functions += IR_WaLBerlaRefinementExclusion2D()
}
