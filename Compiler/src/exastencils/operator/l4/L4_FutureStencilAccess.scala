package exastencils.operator.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureStencilAccess

case class L4_FutureStencilAccess(
    var name : String,
    var level : Int,
    var offset : Option[L4_ConstIndex],
    var dirAccess : Option[L4_ConstIndex],
    var arrayIndex : Option[Int] = None) extends L4_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (dirAccess.isDefined) out << ':' << dirAccess.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
  }

  override def progress = Logger.error(s"Trying to progress future stencil access to $name on level $level")
  def toStencilAccess = L4_StencilAccess(this)
}

/// L4_PrepareStencilAccesses

object L4_PrepareStencilAccesses extends DefaultStrategy("Prepare accesses to stencils") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_StencilCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to stencil ${ access.name }")
      }

      if (access.slot.isDefined) Logger.warn("Discarding meaningless slot access on stencil")

      if (!L4_StencilCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L4_FutureStencilAccess(access.name, lvl, access.offset, access.dirAccess, access.arrayIndex)
  })
}
