package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.ir.IR_FieldAccess

object IR_CollectFieldAccess extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[IR_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[IR_VirtualFieldAccess] = ListBuffer()

  override def apply(node : Option[Node] = None) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.apply(node)
  }

  override def applyStandalone(node : Node) = {
    fieldAccesses.clear
    vFieldAccesses.clear
    super.applyStandalone(node)
  }

  this += new Transformation("Collect", {
    case fieldAccess : IR_FieldAccess        =>
      fieldAccesses += fieldAccess
      fieldAccess
    case fieldAccess : IR_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
