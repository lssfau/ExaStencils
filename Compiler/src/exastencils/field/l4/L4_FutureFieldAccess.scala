package exastencils.field.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureFieldAccess

case class L4_FutureFieldAccess(
    var name : String,
    var level : Int,
    var slot : L4_SlotSpecification,
    var offset : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None) extends L4_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << slot << '@' << level
    if (offset.isDefined) out << '@' << offset
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
  }

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    ??? // TODO
  }

  def toFieldAccess = L4_FieldAccess(this)
}

/// L4_PrepareFieldAccesses

object L4_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field - was an offset access (@) intended?")

      if (!L4_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureFieldAccess(access.name, lvl, access.slot.getOrElse(L4_ActiveSlot), access.offset, access.arrayIndex)
  })
}
