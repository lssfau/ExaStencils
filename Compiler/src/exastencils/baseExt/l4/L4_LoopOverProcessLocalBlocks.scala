package exastencils.baseExt.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_Reduction
import exastencils.base.l4.L4_Statement
import exastencils.baseExt.ir.IR_LoopOverProcessLocalBlocks

trait L4_LoopOverProcessLocalBlocks extends L4_Statement {
  var body : ListBuffer[L4_Statement]
  var reduction : Option[L4_Reduction]

  def progress : IR_LoopOverProcessLocalBlocks
}
