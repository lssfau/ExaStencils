package exastencils.waLBerla.ir.replacements

import exastencils.base.ir._

import exastencils.datastructures.Transformation
import exastencils.globals.ir.IR_AllocateDataFunction
import exastencils.parallelization.ir.IR_HasParallelizationInfo
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaCollection
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInitExaBuffersWrapper

object IR_WaLBerlaReplaceAllocateData extends IR_WaLBerlaReplacementStrategy("Move allocation of waLBerla buffers to waLBerla function") {
  var potentialBlockLoops : Set[IR_ScopedStatement with IR_HasParallelizationInfo] = Set()

  def inAllocateDataFunction(collector : IR_StackCollector) : Boolean = {
    collector.stack.exists {
      case func : IR_Function if IR_AllocateDataFunction.fctName == func.name => true
      case _ => false
    }
  }

  this += Transformation("Collect", {
    case node : IR_Node if inWaLBerlaScope(collector) && inAllocateDataFunction(collector) =>
      potentialBlockLoops += containingWaLBerlaBlockLoop(collector).get
      node
  })

  this += Transformation("Move", {
    case blockLoop : IR_ScopedStatement with IR_HasParallelizationInfo if potentialBlockLoops.contains(blockLoop) =>
      // move
      val funcToMoveTo = IR_WaLBerlaCollection.get.functions.find(_.name == IR_WaLBerlaInitExaBuffersWrapper.fctName).get.asInstanceOf[IR_Function]
      funcToMoveTo.body += blockLoop

      // consume
      None
  })
}
