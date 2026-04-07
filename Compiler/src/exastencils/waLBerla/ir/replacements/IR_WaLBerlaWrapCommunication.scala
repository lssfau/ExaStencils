package exastencils.waLBerla.ir.replacements

import exastencils.base.ir.IR_IfCondition
import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.communication.ir.IR_Communicate
import exastencils.config.Knowledge
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockForest
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

object IR_WaLBerlaWrapCommunication extends IR_WaLBerlaReplacementStrategy("Add guards around waLBerla communication (generated or schemes)") {
  val blockForest = IR_WaLBerlaBlockForest()

  this += Transformation("Find communicate statements with waLBerla field arguments", {
    case comm : IR_Communicate if comm.field.isInstanceOf[IR_WaLBerlaField] || inWaLBerlaScope(collector) =>
      IR_IfCondition(blockForest.getNumberOfAllRootBlocks() > 1 OrOr blockForest.getNumberOfAllLocalBlocks() > 1, comm)
  }, recursive = false)
}
