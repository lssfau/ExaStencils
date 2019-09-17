package exastencils.experimental.ir

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.config.Knowledge
import exastencils.datastructures._
import exastencils.domain.ir.IR_IV_FragmentIndex
import exastencils.hack.ir.HACK_IR_UndeterminedFunctionReference
import exastencils.logger.Logger

/// IR_ResolveGismoFunctions

object IR_ResolveGismoFunctions extends DefaultStrategy("ResolveGismoFunctions") {
  this += new Transformation("ResolveFunctionCalls", {
    case IR_FunctionCall(HACK_IR_UndeterminedFunctionReference("getGismoPatchIdx", _), args) =>
      if (args.nonEmpty) Logger.warn("Ignoring arguments for call to getGismoPatchIdx")

      val lexOrdering = false
      if (lexOrdering) {
        IR_IV_FragmentIndex(0) +
          (1 until Knowledge.dimensionality).map(d =>
            IR_IV_FragmentIndex(d) * (0 until d).map(Knowledge.domain_rect_numFragsTotalAsVec).product : IR_Expression).reduce(_ + _)
      } else {
        IR_IV_FragmentIndex(Knowledge.dimensionality - 1) +
          (0 until Knowledge.dimensionality - 1).map(d =>
            IR_IV_FragmentIndex(d) * (d + 1 until Knowledge.dimensionality).map(Knowledge.domain_rect_numFragsTotalAsVec).product : IR_Expression).reduce(_ + _)
      }
  })
}
