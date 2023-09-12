package exastencils.waLBerla.ir.replacements

import exastencils.baseExt.ir._
import exastencils.datastructures._
import exastencils.fieldlike.ir.IR_FieldLikeAccessLike
import exastencils.util.ir.IR_StackCollector
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaBlockDataID
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlockArray
import exastencils.waLBerla.ir.blockforest.IR_WaLBerlaLoopOverLocalBlocks
import exastencils.waLBerla.ir.field._

abstract class IR_WaLBerlaReplacementStrategy(name : String) extends DefaultStrategy(name) {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  private object IR_WaLBerlaFindAccessed extends QuietDefaultStrategy("Find accessed wb fields") {
    var found = false

    override def applyStandalone(node : Node) : Unit = {
      found = false
      super.applyStandalone(node)
    }

    this += Transformation("Find", {
      case fAcc : IR_FieldLikeAccessLike if IR_WaLBerlaFieldCollection.contains(fAcc) =>
        found = true
        fAcc
      case fAcc : IR_WaLBerlaFieldAccess if IR_WaLBerlaFieldCollection.contains(fAcc) =>
        found = true
        fAcc
      case fAcc : IR_WaLBerlaBlockDataID                                              =>
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

  def containsWaLBerlaFieldAccesses(node : Node) : Boolean = {
    IR_WaLBerlaFindAccessed.applyStandalone(node)
    IR_WaLBerlaFindAccessed.found
  }

  def containingWaLBerlaBlockLoop(collector : IR_StackCollector) = {
    collector.stack.collectFirst {
      case loop : IR_WaLBerlaLoopOverLocalBlocks                                              => loop
      case loop : IR_WaLBerlaLoopOverLocalBlockArray                                          => loop
      case loop : IR_LoopOverPoints if loop.field.isInstanceOf[IR_WaLBerlaField]              => loop
      case loop : IR_LoopOverPointsInOneFragment if loop.field.isInstanceOf[IR_WaLBerlaField] => loop
    }
  }

  def inWaLBerlaBlockLoop(collector : IR_StackCollector) : Boolean = containingWaLBerlaBlockLoop(collector).isDefined
}
