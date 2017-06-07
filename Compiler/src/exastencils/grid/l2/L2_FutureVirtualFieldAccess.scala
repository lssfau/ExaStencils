package exastencils.grid.l2

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.grid.l3.L3_FutureVirtualFieldAccess
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L2_FutureVirtualFieldAccess

case class L2_FutureVirtualFieldAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex] = None) extends L2_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
  }

  def progress = {
    Logger.warn(s"Trying to progress future field access to $name on level $level")
    L3_FutureVirtualFieldAccess(name, level, L2_ProgressOption(offset)(_.progress))
  }

  def toVirtualFieldAccess = L2_VirtualFieldAccess(this)
}

/// L2_PrepareVirtualFieldAccesses

object L2_PrepareVirtualFieldAccesses extends DefaultStrategy("Prepare accesses to virtual fields") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_VirtualFieldCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to field ${ access.name }")
      }

      if (!L2_VirtualFieldCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L2_FutureVirtualFieldAccess(access.name, lvl)
  })
}
