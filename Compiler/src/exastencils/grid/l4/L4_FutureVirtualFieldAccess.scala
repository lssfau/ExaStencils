package exastencils.grid.l4

import exastencils.base.l4._
import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureVirtualFieldAccess

case class L4_FutureVirtualFieldAccess(
    var name : String,
    var level : Int,
    var offset : Option[L4_ConstIndex] = None,
    var arrayIndex : Option[Int] = None) extends L4_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
    if (arrayIndex.isDefined) out << '[' << arrayIndex.get << ']'
  }

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    ??? // TODO
  }

  def toVirtualFieldAccess = L4_VirtualFieldAccess(this)
}

/// L4_PrepareVirtualFieldAccesses

object L4_PrepareVirtualFieldAccesses extends DefaultStrategy("Prepare accesses to virtual fields") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_VirtualFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L4_VirtualFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name } - was an offset access (@) intended?")
      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")

      L4_FutureVirtualFieldAccess(access.name, lvl, access.offset, access.arrayIndex)
  })
}
