package exastencils.field.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l3._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureFieldAccess

case class L2_FutureFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex] = None,
    var frozen : Boolean = false) extends L2_FutureKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    if (frozen) out << "frozen ( "
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (frozen) out << " )"
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L3_FutureFieldAccess(name, level,
      L3_ActiveSlot,
      L2_ProgressOption(offset)(_.progress),
      frozen)
  }

  def toFieldAccess = L2_FieldAccess(this)
}

/// L2_PrepareFieldAccesses

object L2_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L2_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L2_FutureFieldAccess(access.name, lvl, access.offset)
  })
}

/// L2_ResolveFrozenFields

object L2_ResolveFrozenFields extends DefaultStrategy("Resolve frozen field accesses") {
  this += new Transformation("Resolve", {
    case fct : L2_FunctionCall if "frozen" == fct.name =>
      if (fct.arguments.length != 1) Logger.error("Calls to frozen need exactly one argument")
      if (!fct.arguments.head.isInstanceOf[L2_FutureFieldAccess]) Logger.error("Calls to frozen must done with exactly one field access")
      val fieldAccess = fct.arguments.head.asInstanceOf[L2_FutureFieldAccess]
      fieldAccess.frozen = true
      fieldAccess
  })
}
