package exastencils.grid.l3

import exastencils.base.l3.L3_ExpressionIndex
import exastencils.datastructures._
import exastencils.grid.l4._
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

/// L3_VirtualFieldAccess

object L3_VirtualFieldAccess {
  def apply(access : L3_FutureVirtualFieldAccess) =
    new L3_VirtualFieldAccess(L3_VirtualFieldCollection.getByIdentifier(access.name, access.level).get, access.offset)
}

case class L3_VirtualFieldAccess(
    var target : L3_VirtualField,
    var offset : Option[L3_ExpressionIndex] = None) extends L3_LeveledKnowledgeAccess {

  def prettyprint(out : PpStream) = {
    out << target.name << '@' << target.level
    if (offset.isDefined) out << '@' << offset.get
  }

  def progress = L4_VirtualFieldAccess(target.getProgressedObj())
}

/// L3_ResolveVirtualFieldAccesses

object L3_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureVirtualFieldAccess if L3_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess
  })
}
