package exastencils.parallelization.api.cuda

import exastencils.base.ir._
import exastencils.datastructures._

object CUDA_ReplaceArrayAccesses extends QuietDefaultStrategy("Replace array accesses in kernel") {

  var reductionVariable : Option[IR_Expression] = None

  this += new Transformation("Searching", {
    case arrAcc @ IR_ArrayAccess(base : IR_VariableAccess, idx, _) if CUDA_GatherVariableAccesses.containsArrayAccess(base, idx) =>
      val name = CUDA_GatherVariableAccesses.arrayAccessAsString(base, idx)
      val acc = CUDA_GatherVariableAccesses.accesses(name)

      if (reductionVariable.isDefined && reductionVariable.get != arrAcc)
        IR_VariableAccess(name, acc._2)
      else
        arrAcc
  })
}
