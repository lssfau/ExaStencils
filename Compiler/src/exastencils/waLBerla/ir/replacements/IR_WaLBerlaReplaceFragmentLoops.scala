package exastencils.waLBerla.ir.replacements

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.ir._
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.field.IR_WaLBerlaField

object IR_WaLBerlaReplaceFragmentLoops extends IR_WaLBerlaReplacementStrategy("Replace fragment loops over waLBerla fields") {
  var fragmentLoopsToReplace : ListBuffer[IR_LoopOverFragments] = ListBuffer()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    fragmentLoopsToReplace.clear()
    super.apply(applyAtNode)
  }

  this += Transformation("Find remote send/recv calls with accesses to wb fields", {
    case transfer : IR_RemoteTransfer if transfer.field.isInstanceOf[IR_WaLBerlaField] =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      transfer
  })

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments if containsWaLBerlaFieldAccesses(loopOverFrags) || fragmentLoopsToReplace.contains(loopOverFrags) =>
      IR_WaLBerlaLoopOverLocalBlocks(loopOverFrags.body, loopOverFrags.parallelization)
  })
}
