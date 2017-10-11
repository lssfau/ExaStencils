package exastencils.operator.l3

import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l4.L4_FutureStencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_LevelCollector

/// L3_FutureStencilAccess

case class L3_FutureStencilAccess(
    var name : String, var level : Int,
    var offset : Option[L3_ConstIndex],
    var dirAccess : Option[L3_ConstIndex]) extends L3_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
  }

  def progress = {
    Logger.warn(s"Trying to progress future stencil access to $name on level $level")
    L4_FutureStencilAccess(name, level,
      L3_ProgressOption(offset)(_.progress),
      L3_ProgressOption(dirAccess)(_.progress))
  }

  def toStencilAccess = L3_StencilAccess(this)
}

/// L3_PrepareStencilAccesses

object L3_PrepareStencilAccesses extends DefaultStrategy("Prepare accesses to stencils") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_StencilCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil ${ access.name }")
      }

      if (!L3_StencilCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L3_FutureStencilAccess(access.name, lvl, access.offset, access.dirAccess)
  })
}
