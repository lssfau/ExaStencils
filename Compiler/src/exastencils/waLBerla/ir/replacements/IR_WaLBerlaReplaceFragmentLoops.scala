package exastencils.waLBerla.ir.replacements

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.communication.ir._
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.parallelization.api.cuda._
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.field.IR_WaLBerlaField
import exastencils.waLBerla.ir.interfacing.IR_WaLBerlaInterfaceMember

object IR_WaLBerlaPrepareReplaceFragmentLoops extends IR_WaLBerlaReplacementStrategy("Prepare fragment loops replacements with waLBerla block loops") {
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

  this += Transformation("Find remote wait calls with accesses to wb fields", {
    case wait : IR_WaitForRemoteTransfer if wait.field.isInstanceOf[IR_WaLBerlaField] =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      wait
  })

  this += Transformation("Find host/device dirty flag queries with accesses to wb fields", {
    case dirtyFlag : CUDA_HostBufferDataUpdated if dirtyFlag.field.isInstanceOf[IR_WaLBerlaField]   =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      dirtyFlag
    case dirtyFlag : CUDA_DeviceBufferDataUpdated if dirtyFlag.field.isInstanceOf[IR_WaLBerlaField] =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      dirtyFlag
    case dirtyFlag : CUDA_HostDataUpdated if dirtyFlag.field.isInstanceOf[IR_WaLBerlaField]         =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      dirtyFlag
    case dirtyFlag : CUDA_DeviceDataUpdated if dirtyFlag.field.isInstanceOf[IR_WaLBerlaField]       =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      dirtyFlag
  })

  this += Transformation("Find loops accessing wb IVs", {
    case iv : IR_WaLBerlaInterfaceMember =>
      fragmentLoopsToReplace ++= collector.stack.collectFirst { case n : IR_LoopOverFragments => n }
      iv
  })
}

object IR_WaLBerlaReplaceFragmentLoops extends IR_WaLBerlaReplacementStrategy("Replace fragment loops over waLBerla fields") {
  this.onBefore = () => IR_WaLBerlaPrepareReplaceFragmentLoops.apply()

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments if containsWaLBerlaFieldAccesses(loopOverFrags) ||
      IR_WaLBerlaPrepareReplaceFragmentLoops.fragmentLoopsToReplace.contains(loopOverFrags) =>

      IR_WaLBerlaLoopOverLocalBlocks(loopOverFrags.body, loopOverFrags.parallelization)
  })
}
