package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.core.Duplicate
import exastencils.datastructures._

object CUDA_ReplaceArrayAccesses extends QuietDefaultStrategy("Replace array accesses in kernel") {

  var reductionTarget : Option[IR_Expression] = None

  def isReductionTarget(arrAcc : IR_ArrayAccess) = {
    def equalsReductionVariable(expr : IR_Expression) = reductionTarget.isDefined && reductionTarget.get == expr

    arrAcc.base match {
      case vAcc : IR_VariableAccess => equalsReductionVariable(arrAcc) || equalsReductionVariable(vAcc)
      case _                        => false
    }
  }

  this += new Transformation("Searching", {

    // handling for compound assignments
    case IR_Assignment(dst @ IR_ArrayAccess(base : IR_VariableAccess, idx, _), src, op) if List("+=", "-=", "*=", "/=").contains(op) && isReductionTarget(dst) =>
      // only replace access for rhs
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)
      val replacement = IR_VariableAccess(name, acc._2)

      IR_Assignment(dst, IR_BinaryOperators.createExpression(op.replace("=", ""), replacement, src))

    case assign @ IR_Assignment(dst @ IR_ArrayAccess(_ : IR_VariableAccess, _, _), src, "=") if isReductionTarget(dst) =>
      // do not consider reduction variable for replacement on rhs
      val tmp = Duplicate(reductionTarget.get)
      reductionTarget = None
      CUDA_ReplaceArrayAccesses.applyStandalone(src)
      reductionTarget = Some(tmp)

      assign

    // replace array access if not the reduction target
    case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.containsArrayAccess(base, idx) =>
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)

      if (!isReductionTarget(arrAcc))
        IR_VariableAccess(name, acc._2)
      else
        arrAcc
  })
}
