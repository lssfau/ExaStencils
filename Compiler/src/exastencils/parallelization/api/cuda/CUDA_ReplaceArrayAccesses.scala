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

  object CUDA_ReplaceReductionVariableAccesses extends QuietDefaultStrategy("") {
    this += Transformation("Replace accesses to reduction vars", {

      case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.containsArrayAccess(base, idx) =>
        val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
        val acc = CUDA_GatherVariableAccesses.accesses(name)

        // replace array access if not the reduction target
        if (!isReductionTarget(reductionTarget, arrAcc)) {
          IR_VariableAccess(name, acc._2)
        } else {
          arrAcc
        }
    })
  }

  this += new Transformation("Searching", {
    // handling for assignments containing the reduction variable
    case IR_Assignment(dst @ IR_ArrayAccess(base : IR_VariableAccess, idx, _), src, op) if List("+=", "-=", "*=", "/=").contains(op) && isReductionVariableAccess(reductionTarget, dst) =>
      // handling for compound assignments
      // only replace access for rhs
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)
      val replacement = IR_VariableAccess(name, acc._2)

      IR_Assignment(dst, IR_BinaryOperators.createExpression(op.replace("=", ""), replacement, src))

    case _ @ IR_Assignment(dst @ IR_ArrayAccess(_ : IR_VariableAccess, _, _), src, "=") if isReductionVariableAccess(reductionTarget, dst) =>

      // allow reduction variable on rhs to be replaced
      src match {
        // replace immediately
        case _ @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if isReductionVariableAccess(reductionTarget, dst) && CUDA_GatherVariableAccesses.containsArrayAccess(base, idx) =>
          val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
          val acc = CUDA_GatherVariableAccesses.accesses(name)
          val replacement = IR_VariableAccess(name, acc._2)

          IR_Assignment(dst, replacement)
        // traverse tree and replace
        case _  : IR_Expression =>
          val tmp = Duplicate(reductionTarget.get)
          reductionTarget = None
          CUDA_ReplaceReductionVariableAccesses.applyStandalone(src)
          reductionTarget = Some(tmp)

          IR_Assignment(dst, src)
      }

    case e : IR_MatrixExpression =>
      // traverse tree and replace
      CUDA_ReplaceReductionVariableAccesses.applyStandalone(e)
      e
  })
}
