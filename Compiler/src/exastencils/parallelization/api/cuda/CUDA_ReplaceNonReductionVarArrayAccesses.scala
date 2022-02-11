package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Expression
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.datastructures._
import exastencils.parallelization.api.cuda.CUDA_Util._

/// CUDA_ReplaceNonReductionVarArrayAccesses

// replace array accesses in kernel from host arrays to either kernel args or device buffers
// do not replace the reduction variable for the sake of a (later) replacement strategy (CUDA_HandleReductions)

object CUDA_ReplaceNonReductionVarArrayAccesses extends QuietDefaultStrategy("Replace array accesses in kernel") {

  var reductionTarget : Option[IR_Expression] = None

  this += new Transformation("Searching", {
    // special case: unresolved matrix expressions
    case e : IR_MatrixExpression =>
      // traverse tree and replace
      CUDA_ReplaceNonReductionVarArrayAccesses.applyStandalone(e)
      e

    case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.isReplaceable(base, idx) =>
      // replace array access if not access to the reduction target
      if (!isReductionTarget(reductionTarget, arrAcc) && !isReductionVariableAccess(reductionTarget, arrAcc)) {
        CUDA_GatherVariableAccesses.replaceAccess(base, idx).get
      } else {
        arrAcc
      }
  })
}
