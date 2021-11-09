package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_Root
import exastencils.baseExt.ir.IR_LoopOverDimensions
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation

object IR_WaLBerlaCollectAccessedFields extends DefaultStrategy("Collect waLBerla field accesses") {
  var wbFieldAccesses : ListBuffer[IR_WaLBerlaFieldAccess] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    wbFieldAccesses.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    wbFieldAccesses.clear
    super.applyStandalone(node)
  }

  def applyWrappedStandalone(node : IR_Node) = {
    applyStandalone(IR_Root(node))
  }

  this += new Transformation("Collect", {
    case fieldAccess : IR_WaLBerlaFieldAccess =>
      wbFieldAccesses += fieldAccess
      fieldAccess
    case swap : IR_WaLBerlaSwapFieldPointers =>
      wbFieldAccesses ++= List(swap.src, swap.dst)
      swap
    case access : IR_IV_WaLBerlaFieldData =>
      wbFieldAccesses += IR_WaLBerlaFieldAccess(access.field, access.slot, IR_LoopOverDimensions.defIt(access.field.numDimsGrid))
      access
  })
}