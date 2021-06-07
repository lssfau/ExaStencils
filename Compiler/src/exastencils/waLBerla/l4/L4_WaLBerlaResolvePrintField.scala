package exastencils.waLBerla.l4

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Transformation
import exastencils.field.l4.L4_PrintField

object L4_WaLBerlaResolvePrintField extends DefaultStrategy("Resolve printField for waLBerla fields") {
  var printStmts : ListBuffer[L4_PrintField] = ListBuffer()

  this += Transformation("Gather print stmts", {
    case print @ L4_PrintField(_, fAcc, _, _, _, _) if L4_WaLBerlaFieldCollection.contains(fAcc) =>
      printStmts += print
      print
  })

  this += Transformation("Wrap around loopOverBlock", {
    case print : L4_PrintField if printStmts.contains(print) =>
      printStmts -= print
      L4_WaLBerlaLoopOverBlocks(mutable.ListBuffer(print), reduction = None)
  })
}
