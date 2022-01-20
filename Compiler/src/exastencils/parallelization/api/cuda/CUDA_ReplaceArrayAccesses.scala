package exastencils.parallelization.api.cuda

import exastencils.base.ir.IR_ArrayAccess
import exastencils.base.ir.IR_Expression
import exastencils.base.ir._
import exastencils.baseExt.ir.IR_MatrixExpression
import exastencils.core.Duplicate
import exastencils.datastructures._
import exastencils.parallelization.api.cuda.CUDA_Util._

object CUDA_ReplaceArrayAccesses extends QuietDefaultStrategy("Replace array accesses in kernel") {

  var reductionTarget : Option[IR_Expression] = None
  var kernelCount : Int = 0

  private val compoundAssignmentOps = List("+=", "-=", "*=", "/=")

  object CUDA_ReplaceNonReductionVariableAccesses extends QuietDefaultStrategy("Replace accesses to non-reduction vars") {
    /* replace array access of non reduction targets as follows:
      - evaluable index -> variable access (kernel argument)
      - non-evaluable index -> change base of array access to a device copy (pointer passed as kernel argument) */
    this += Transformation("..", {
      case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.isReplaceable(base, idx) =>
        // replace array access if not the reduction target
        if (!isReductionTarget(reductionTarget, arrAcc)) {
          CUDA_GatherVariableAccesses.replaceAccess(base, idx).get
        } else {
          arrAcc
        }
    })
  }

  this += new Transformation("Searching", {
    /* handling for assignments containing the reduction variable
       -> do not replace the reduction variable on the lhs for the sake of another replacement strategy (CUDA_HandleReductions),
       where reduction variable access are replaced with device reduction buffer accesses
       -> only replace for the rhs (reduction target accesses are replaced with kernel arg accesses) */
    case assign @ IR_Assignment(dst @ IR_ArrayAccess(base : IR_VariableAccess, idx, _), src, op) if compoundAssignmentOps.contains(op) && isReductionVariableAccess(reductionTarget, dst) =>
      // handling for compound assignments
      if (CUDA_GatherVariableAccesses.isReplaceable(base, idx))
        IR_Assignment(dst, IR_BinaryOperators.createExpression(op.replace("=", ""), CUDA_GatherVariableAccesses.replaceAccess(base, idx).get, src))
      else
        assign
    case _ @ IR_Assignment(dst @ IR_ArrayAccess(_ : IR_VariableAccess, _, _), src, "=") if isReductionVariableAccess(reductionTarget, dst) =>
      // regular assignments: only allow reduction variable on rhs to be replaced
      src match {
        // replace immediately
        case _ @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.isReplaceable(base, idx) =>
          IR_Assignment(dst, CUDA_GatherVariableAccesses.replaceAccess(base, idx).get)
        // traverse tree and replace
        case _  : IR_Expression =>
          // allow replacement of reduction variables on rhs
          val tmp = Duplicate(reductionTarget.get)
          reductionTarget = None
          CUDA_ReplaceNonReductionVariableAccesses.applyStandalone(src)
          reductionTarget = Some(tmp)

          IR_Assignment(dst, src)
      }
    /* lhs is not the reduction variable:
      -> no special handling needed
      -> simply replace accesses on rhs */
    case assign @ IR_Assignment(dst, src, _) =>
      var tmp : Option[IR_Expression] = None
      if (reductionTarget.isDefined)
        tmp = Some(Duplicate(reductionTarget.get))
      reductionTarget = None

      CUDA_ReplaceNonReductionVariableAccesses.applyStandalone(src)

      reductionTarget = tmp

      assign

    // special case: unresolved matrix expressions
    case e : IR_MatrixExpression =>
      // traverse tree and replace
      CUDA_ReplaceNonReductionVariableAccesses.applyStandalone(e)
      e
  })
}
