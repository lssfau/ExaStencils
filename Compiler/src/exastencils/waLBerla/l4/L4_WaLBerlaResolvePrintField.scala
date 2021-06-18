package exastencils.waLBerla.l4

import scala.collection.mutable

import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_PrintField
import exastencils.util.l4.L4_StackCollector

object L4_WaLBerlaResolvePrintField extends DefaultStrategy("Resolve printField for waLBerla fields") {
  val collector = new L4_StackCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += Transformation("Gather print stmts", {
    case print @ L4_PrintField(_, fAcc, _, _, _, _) if L4_WaLBerlaFieldCollection.contains(fAcc) =>
      if (!collector.stack.exists(_.isInstanceOf[L4_WaLBerlaLoopOverBlocks]))
        L4_WaLBerlaLoopOverBlocks(mutable.ListBuffer(print), reduction = None)
      else
        print
  })
}
