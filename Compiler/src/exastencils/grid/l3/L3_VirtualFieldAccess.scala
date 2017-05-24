package exastencils.grid.l3

import exastencils.base.l4.L4_SingleLevel
import exastencils.datastructures._
import exastencils.grid.l4.L4_VirtualFieldAccess
import exastencils.knowledge.l3.L3_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

/// L3_VirtualFieldAccess

object L3_VirtualFieldAccess {
  def apply(access : L3_FutureVirtualFieldAccess) =
    new L3_VirtualFieldAccess(L3_VirtualFieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L3_VirtualFieldAccess(var target : L3_VirtualField) extends L3_LeveledKnowledgeAccess {
  def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L4_VirtualFieldAccess(target.name, L4_SingleLevel(target.level))
}

/// L3_ResolveVirtualFieldAccesses

object L3_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureVirtualFieldAccess if L3_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess
  })
}
