package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.baseExt.ir.IR_LoopOverFragments
import exastencils.datastructures.Node
import exastencils.datastructures.QuietDefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_FieldAccessLike
import exastencils.util.ir.IR_StackCollector

object IR_WaLBerlaReplaceFragmentLoops extends QuietDefaultStrategy("Replace fragment loops over waLBerla fields") {
  var collector = new IR_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  var fragLoops : ListBuffer[IR_LoopOverFragments] = ListBuffer()

  override def apply(applyAtNode : Option[Node]) : Unit = {
    fragLoops.clear()
    super.apply(applyAtNode)
  }

  override def applyStandalone(node : Node) : Unit = {
    fragLoops.clear()
    super.applyStandalone(node)
  }

  this += Transformation("Omit fragment loops within block loops", {
    case loopOverFrags : IR_LoopOverFragments if collector.stack.exists(_.isInstanceOf[IR_WaLBerlaLoopOverBlocks]) =>
      loopOverFrags.body
  })

  this += Transformation("Collect", {
    case fAcc : IR_FieldAccessLike if IR_WaLBerlaFieldCollection.contains(fAcc) =>
      fragLoops ++= collector.stack.collect{ case a : IR_LoopOverFragments => a }
      fAcc
  })

  this += Transformation("Replace", {
    case loopOverFrags : IR_LoopOverFragments if fragLoops.contains(loopOverFrags) =>
      IR_WaLBerlaLoopOverBlocks(loopOverFrags.body, loopOverFrags.parallelization)
  })
}
