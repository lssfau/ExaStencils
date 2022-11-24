package exastencils.waLBerla.ir

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccessLike
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverBlocks
import exastencils.waLBerla.ir.field._

object IR_WaLBerlaReplaceFragmentLoops extends QuietDefaultStrategy("Replace fragment loops over waLBerla fields") {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  object IR_WaLBerlaFindAccessed extends QuietDefaultStrategy("Find accessed wb fields") {
    var found = false

    override def apply(applyAtNode : Option[Node]) : Unit = {
      found = false
      super.apply(applyAtNode)
    }

    override def applyStandalone(node : Node) : Unit = {
      found = false
      super.applyStandalone(node)
    }

    this += Transformation("Find", {
      case fAcc : IR_FieldAccessLike if IR_WaLBerlaFieldCollection.contains(fAcc)     =>
        found = true
        fAcc
      case fAcc : IR_WaLBerlaFieldAccess if IR_WaLBerlaFieldCollection.contains(fAcc) =>
        found = true
        fAcc
      case fAcc : IR_IV_WaLBerlaGetField                                              =>
        found = true
        fAcc
      case fAcc : IR_IV_WaLBerlaFieldData                                             =>
        found = true
        fAcc
    })
  }

  this += Transformation("Omit fragment loops within block loops", {
    case loopOverFrags : IR_LoopOverFragments if collector.stack.exists(_.isInstanceOf[IR_WaLBerlaLoopOverBlocks]) =>
      loopOverFrags.body
  })

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments =>
      IR_WaLBerlaFindAccessed.applyStandalone(loopOverFrags)
      if (IR_WaLBerlaFindAccessed.found)
        IR_WaLBerlaLoopOverBlocks(loopOverFrags.body, loopOverFrags.parallelization)
      else
        loopOverFrags
  })
}
