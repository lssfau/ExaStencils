package exastencils.field.l3

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l3.L3_FieldCollection
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

case class L3_FieldAccess(
    var field : L3_Field,
    var level : Int) extends L3_Access {

  override def name = field.name
  override def prettyprint(out : PpStream) = out << name << '@' << level
  override def progress = L4_FieldAccess(field.getProgressedObject(), L4_ActiveSlot)
}

/// L3_ResolveFieldAccesses

object L3_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_FieldCollection.exists(access.name) =>
      val level = if (access.level.isDefined) access.level.get.resolveLevel else collector.getCurrentLevel
      val field = L3_FieldCollection.getByIdentifier(access.name, level).get
      L3_FieldAccess(field, level)
  })
}
