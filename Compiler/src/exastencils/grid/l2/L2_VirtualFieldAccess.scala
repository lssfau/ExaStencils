package exastencils.grid.l2

import exastencils.datastructures._
import exastencils.grid.l3.L3_VirtualFieldAccess
import exastencils.knowledge.l2.L2_LeveledKnowledgeAccess
import exastencils.prettyprinting.PpStream

/// L2_VirtualFieldAccess

object L2_VirtualFieldAccess {
  def apply(name : String, level : Int) =
    new L2_VirtualFieldAccess(L2_VirtualFieldCollection.getByIdentifier(name, level).get)

  def apply(access : L2_FutureVirtualFieldAccess) =
    new L2_VirtualFieldAccess(L2_VirtualFieldCollection.getByIdentifier(access.name, access.level).get)
}

case class L2_VirtualFieldAccess(var target : L2_VirtualField) extends L2_LeveledKnowledgeAccess {
  def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  def progress = L3_VirtualFieldAccess(target.name, target.level)
}

/// L2_ResolveVirtualFieldAccesses

object L2_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureVirtualFieldAccess if L2_VirtualFieldCollection.exists(access.name, access.level) =>
      access.toVirtualFieldAccess
  })
}

///// L2_ResolveVirtualFieldAccesses
//
//object L2_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
//  val collector = new L2_LevelCollector
//  this.register(collector)
//
//  this += new Transformation("Resolve applicable unresolved accesses", {
//    case access : L2_UnresolvedAccess if VirtualField.fields.contains(access.name.toLowerCase()) =>
//      val level = if (access.level.isDefined) access.level.get.resolveLevel else collector.getCurrentLevel
//      L2_VirtualFieldAccess(access.name, level)
//  })
//}
