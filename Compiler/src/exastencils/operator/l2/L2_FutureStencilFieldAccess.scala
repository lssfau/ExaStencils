package exastencils.operator.l2

import exastencils.base.ProgressLocation
import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l3.L3_FutureStencilFieldAccess
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureStencilFieldAccess

case class L2_FutureStencilFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex],
    var dirAccess : Option[L2_ConstIndex]) extends L2_FutureKnowledgeAccess with L2_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future stencil field access to $name on level $level")
    L3_FutureStencilFieldAccess(name, level,
      L2_ProgressOption(offset)(_.progress),
      L2_ProgressOption(dirAccess)(_.progress))
  }

  def toStencilFieldAccess = L2_StencilFieldAccess(this)
}

/// L2_PrepareStencilFieldAccesses

object L2_PrepareStencilFieldAccesses extends DefaultStrategy("Prepare accesses to stencil fields") {
  val collector = new L2_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_StencilFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil field ${ access.name }")
      }

      if (!L2_StencilFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L2_FutureStencilFieldAccess(access.name, lvl, access.offset, access.dirAccess)
  })
}
