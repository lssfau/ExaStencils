package exastencils.waLBerla.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_Node
import exastencils.base.ir.IR_Root
import exastencils.datastructures.DefaultStrategy
import exastencils.datastructures.Node
import exastencils.datastructures.Transformation
import exastencils.field.ir.IR_Field
import exastencils.field.ir.IR_IV_FieldData

object IR_CollectAccessedWaLBerlaFields extends DefaultStrategy("Collect waLBerla field accesses") {
  var wbFieldAccesses : ListBuffer[IR_WaLBerlaField] = ListBuffer()

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
      wbFieldAccesses += fieldAccess.field
      fieldAccess
    case swap : IR_WaLBerlaSwapFieldPointers  =>
      wbFieldAccesses ++= List(swap.wbSrc.field, swap.wbDst.field)
      swap
    case access : IR_IV_WaLBerlaFieldData     =>
      wbFieldAccesses += access.field
      access
  })
}

object IR_CollectAccessedLowerLevelWaLBerlaFields extends DefaultStrategy("Collect lower-level waLBerla field accesses") {
  var fieldAccesses : ListBuffer[IR_Field] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    fieldAccesses.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    fieldAccesses.clear
    super.applyStandalone(node)
  }

  def applyWrappedStandalone(node : IR_Node) = {
    applyStandalone(IR_Root(node))
  }

  this += new Transformation("Collect", {
    case access : IR_IV_FieldData     =>
      fieldAccesses += access.field
      access
  })
}