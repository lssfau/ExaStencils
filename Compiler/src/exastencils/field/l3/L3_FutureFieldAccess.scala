package exastencils.field.l3

import exastencils.base.l3.L3_LevelCollector
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_FutureFieldAccess

case class L3_FutureFieldAccess(var name : String, var level : Int) extends L3_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    ??? // TODO
  }

  def toFieldAccess = L3_FieldAccess(this)
}

/// L3_PrepareFieldAccesses

object L3_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L3_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L3_FutureFieldAccess(access.name, lvl)
  })
}

