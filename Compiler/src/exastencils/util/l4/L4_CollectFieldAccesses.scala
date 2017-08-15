package exastencils.util.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l4.L4_FieldAccess
import exastencils.grid.l4.L4_VirtualFieldAccess

/// L4_CollectFieldAccesses

object L4_CollectFieldAccesses extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[L4_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[L4_VirtualFieldAccess] = ListBuffer()

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

  def applyWrappedStandalone(node : L4_Node) = {
    applyStandalone(L4_Root(node))
  }

  this += new Transformation("Collect", {
    case fieldAccess : L4_FieldAccess =>
      fieldAccesses += fieldAccess
      fieldAccess

    case fieldAccess : L4_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
