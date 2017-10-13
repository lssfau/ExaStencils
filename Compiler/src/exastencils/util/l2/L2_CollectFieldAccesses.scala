package exastencils.util.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.datastructures.Transformation._
import exastencils.datastructures._
import exastencils.field.l2.L2_FieldAccess
import exastencils.grid.l2.L2_VirtualFieldAccess

/// L2_CollectFieldAccesses

object L2_CollectFieldAccesses extends QuietDefaultStrategy("Collect field accesses") {
  var fieldAccesses : ListBuffer[L2_FieldAccess] = ListBuffer()
  var vFieldAccesses : ListBuffer[L2_VirtualFieldAccess] = ListBuffer()

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

  def applyWrappedStandalone(node : L2_Node) = {
    applyStandalone(L2_Root(node))
  }

  this += new Transformation("Collect", {
    case fieldAccess : L2_FieldAccess =>
      fieldAccesses += fieldAccess
      fieldAccess

    case fieldAccess : L2_VirtualFieldAccess =>
      vFieldAccesses += fieldAccess
      fieldAccess
  })
}
