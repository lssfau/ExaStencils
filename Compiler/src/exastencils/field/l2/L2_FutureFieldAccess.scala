package exastencils.field.l2

import exastencils.base.l2.L2_LevelCollector
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_FutureFieldAccess

case class L2_FutureFieldAccess(var name : String, var level : Int) extends L2_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    ??? // TODO
  }

  def toFieldAccess = L2_FieldAccess(this)
}

/// L2_PrepareFieldAccesses

object L2_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L2_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L2_FutureFieldAccess(access.name, lvl)
  })
}
