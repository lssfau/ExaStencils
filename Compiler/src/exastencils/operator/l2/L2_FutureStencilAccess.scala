package exastencils.operator.l2

import exastencils.base.l2.L2_LevelCollector
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_FutureStencilAccess

case class L2_FutureStencilAccess(var name : String, var level : Int) extends L2_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future stencil access to $name on level $level")
    ??? // TODO
  }

  def toStencilAccess = L2_StencilAccess(this)
}

/// L2_PrepareStencilAccesses

object L2_PrepareStencilAccesses extends DefaultStrategy("Prepare accesses to stencils") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_StencilCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil ${ access.name }")
      }

      if (!L2_StencilCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L2_FutureStencilAccess(access.name, lvl)
  })
}
