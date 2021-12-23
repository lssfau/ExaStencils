package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

object CUDA_ReplaceArrayAccesses extends QuietDefaultStrategy("Replace array accesses in kernel") {

  var reductionTarget : Option[IR_Expression] = None

  def isReductionVariable(expr : IR_Expression) = reductionTarget.isDefined && reductionTarget.get == expr

  this += new Transformation("Searching", {

    // handling for compound assignments
    case IR_Assignment(dst @ IR_ArrayAccess(base : IR_VariableAccess, idx, _), src, op) if List("+=", "-=", "*=", "/=").contains(op) && isReductionVariable(dst) =>
      // only replace access for rhs
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)
      val replacement = IR_VariableAccess(name, acc._2)

      IR_Assignment(dst, IR_BinaryOperators.createExpression(op.replace("=", ""), replacement, src))

    case assign @ IR_Assignment(dst, src, "=") if isReductionVariable(dst) =>
      // do not consider reduction variable for replacement on rhs
      val tmp = Duplicate(reductionTarget.get)
      reductionTarget = None
      CUDA_ReplaceArrayAccesses.applyStandalone(src)
      reductionTarget = Some(tmp)

      assign

    // replace array access if not the reduction target
    case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.containsArrayAccess(base, idx) && !isReductionVariable(arrAcc) =>
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)

      IR_VariableAccess(name, acc._2)
  })
}
