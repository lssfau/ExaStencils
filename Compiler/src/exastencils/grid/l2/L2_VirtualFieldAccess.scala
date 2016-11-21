package exastencils.grid.l2

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.grid.VirtualField
import exastencils.grid.l3.L3_VirtualFieldAccess
import exastencils.prettyprinting.PpStream

case class L2_VirtualFieldAccess(var name : String, var level : Int) extends L2_Access {
  def prettyprint(out : PpStream) = out << name << '@' << level
  def progress = L3_VirtualFieldAccess(name, level)
}

/// L2_ResolveVirtualFieldAccesses

object L2_ResolveVirtualFieldAccesses extends DefaultStrategy("Resolve accesses to virtual fields") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if VirtualField.fields.contains(access.name.toLowerCase()) =>
      val level = if (access.level.isDefined) access.level.get.resolveLevel else collector.getCurrentLevel
      L2_VirtualFieldAccess(access.name, level)
  })
}
