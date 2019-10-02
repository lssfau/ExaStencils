package exastencils.baseExt.ir

import exastencils.base.ir._
import exastencils.datastructures._
import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference

/// IR_InferDiagAndInverseCallDataTypes

object IR_InferDiagAndInverseCallDataTypes extends DefaultStrategy("InferDiagAndInverseCallDataTypes") {
  var _changed = 0

  var fcts = List("diag", "inverse")

  def doUntilDone(node : Option[Node] = None) = {
    do {
      _changed = 0
      apply(node)
    } while (_changed > 0)
  }

  this += Transformation("do", {
    case call @ IR_FunctionCall(ref : HACK_IR_UndeterminedFunctionReference, params)
      if fcts.contains(ref.name) && call.datatype == IR_UnknownDatatype && params.head.datatype != IR_UnknownDatatype =>

      _changed += 1
      call.function = IR_PlainInternalFunctionReference(ref.name, params.head.datatype)
      call
  })
}
