package exastencils.field.l4

import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureFieldLayoutAccess

case class L4_FutureFieldLayoutAccess(var name : String, var level : Int) extends L4_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future field layout access to $name on level $level")
    ??? // TODO
  }

  def toFieldLayoutAccess = L4_FieldLayoutAccess(this)
}

/// L4_PrepareFieldLayoutAccesses

object L4_PrepareFieldLayoutAccesses extends DefaultStrategy("Prepare accesses to field layouts") {
  val collector = new L4_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_FieldLayoutCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field layout ${ access.name }")
      }

      if (access.offset.isDefined) Logger.warn("Discarding meaningless offset access on field layout")
      if (access.dirAccess.isDefined) Logger.warn("Discarding meaningless direction access on field layout")

      if (!L4_FieldLayoutCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureFieldLayoutAccess(access.name, lvl)
  })
}
