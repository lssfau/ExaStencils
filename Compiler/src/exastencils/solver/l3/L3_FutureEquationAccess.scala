package exastencils.solver.l3

import exastencils.base.ProgressLocation
import exastencils.base.l3._
import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.solver.l4.L4_FutureEquationAccess
import exastencils.util.l3.L3_LevelCollector

/// L3_FutureEquationAccess

case class L3_FutureEquationAccess(
    var name : String, var level : Int,
    var offset : Option[L3_ConstIndex] = None) extends L3_FutureKnowledgeAccess with L3_CanBeOffset {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
  }

  override def progress = ProgressLocation {
    Logger.warn(s"Trying to progress future equation access to $name on level $level")
    L4_FutureEquationAccess(name, level, L3_ProgressOption(offset)(_.progress))
  }

  def toEquationAccess = L3_EquationAccess(this)
}

/// L3_PrepareEquationAccesses

object L3_PrepareEquationAccesses extends DefaultStrategy("Prepare accesses to equations") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_EquationCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to equation ${ access.name }")
      }

      if (!L3_EquationCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L3_FutureEquationAccess(access.name, lvl, access.offset)
  })
}
