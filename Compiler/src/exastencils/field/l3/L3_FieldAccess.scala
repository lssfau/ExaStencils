package exastencils.field.l3

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l4._
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_FieldAccess

object L3_FieldAccess {
  def apply(stencilName : String, level : Int) =
    new L3_FieldAccess(L3_FieldCollection.getByIdentifier(stencilName, level).get)
}

case class L3_FieldAccess(var target : L3_Field) extends L3_KnowledgeAccess {
  override def name = target.name
  override def prettyprint(out : PpStream) = out << target.name << '@' << target.level
  override def progress = L4_FieldAccess(target.getProgressedObject(), L4_ActiveSlot)
}

/// L3_ResolveFieldAccesses

object L3_ResolveFieldAccesses extends DefaultStrategy("Resolve accesses to fields") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_FieldCollection.exists(access.name) =>
      val level = if (access.level.isDefined) access.level.get.resolveLevel else collector.getCurrentLevel
      val field = L3_FieldCollection.getByIdentifier(access.name, level).get
      L3_FieldAccess(field)
  })
}
