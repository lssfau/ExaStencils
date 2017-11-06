package exastencils.field.l1

import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.field.l2.L2_FutureFieldAccess
import exastencils.knowledge.l1.L1_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_LevelCollector

/// L1_FutureFieldAccess

case class L1_FutureFieldAccess(var name : String, var level : Int) extends L1_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L2_FutureFieldAccess(name, level, None)
  }

  def toFieldAccess = L1_FieldAccess(this)
}

/// L1_PrepareFieldAccesses

object L1_PrepareFieldAccesses extends DefaultStrategy("Prepare accesses to fields") {
  val collector = new L1_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L1_UnresolvedAccess if L1_FieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L1_FieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L1_FutureFieldAccess(access.name, lvl)
  })
}
