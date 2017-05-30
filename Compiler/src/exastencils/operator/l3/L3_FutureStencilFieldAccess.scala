package exastencils.operator.l3

import exastencils.base.l3.L3_LevelCollector
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L3_FutureStencilFieldAccess

case class L3_FutureStencilFieldAccess(var name : String, var level : Int) extends L3_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future stencil field access to $name on level $level")
    ??? // TODO
  }

  def toStencilFieldAccess = L3_StencilFieldAccess(this)
}

/// L3_PrepareStencilFieldAccesses

object L3_PrepareStencilFieldAccesses extends DefaultStrategy("Prepare accesses to stencil fields") {
  val collector = new L3_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_StencilFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil field ${ access.name }")
      }

      if (!L3_StencilFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L3_FutureStencilFieldAccess(access.name, lvl)
  })
}
