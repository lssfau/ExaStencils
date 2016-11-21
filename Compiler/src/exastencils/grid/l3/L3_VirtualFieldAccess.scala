package exastencils.grid.l3

import exastencils.base.l3._
import exastencils.base.l4._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.grid.VirtualField
import exastencils.grid.l4.L4_VirtualFieldAccess
import exastencils.prettyprinting.PpStream

case class L3_VirtualFieldAccess(var name : String, var level : Int) extends L3_Access {
  def prettyprint(out : PpStream) = out << name << '@' << level
  def progress = L4_VirtualFieldAccess(name, L4_SingleLevel(level), None, None)
}

/// L3_ResolveVirtualFieldAccesses

object L3_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if VirtualField.fields.contains(access.name.toLowerCase()) =>
      val level = if (access.level.isDefined) access.level.get.resolveLevel else collector.getCurrentLevel
      L3_VirtualFieldAccess(access.name, level)
  })
}
