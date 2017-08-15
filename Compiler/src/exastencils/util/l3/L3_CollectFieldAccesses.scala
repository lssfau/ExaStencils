package exastencils.util.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l3.L3_FieldAccess
import exastencils.grid.l3.L3_VirtualFieldAccess

/// L3_CollectFieldAccesses

object L3_CollectFieldAccesses extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[L3_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[L3_VirtualFieldAccess] = ListBuffer()

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

  def applyWrappedStandalone(node : L3_Node) = {
    applyStandalone(L3_Root(node))
  }

  this += new Transformation("Collect", {
    case fieldAccess : L3_FieldAccess =>
      fieldAccesses += fieldAccess
      fieldAccess

    case fieldAccess : L3_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
