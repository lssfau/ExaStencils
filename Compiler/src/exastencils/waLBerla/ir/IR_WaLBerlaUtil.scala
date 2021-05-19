package exastencils.waLBerla.ir

import exastencils.base.ir.IR_Function
import exastencils.base.ir.IR_LeveledFunction
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.logger.Logger

object IR_WaLBerlaUtil extends DefaultStrategy("Get waLBerla sweep") {
  def isWaLBerlaKernel(func : IR_Function) : Boolean = func.name.startsWith("walberla_")
  var startNode : Option[IR_LeveledFunction] = None

  this += Transformation("Get sweep node", {
    case func : IR_LeveledFunction if isWaLBerlaKernel(func) =>
      if (startNode.isEmpty)
        startNode = Some(func)
      else
        Logger.error("Multiple waLBerla sweep candidates found.")

      func
  })
}
