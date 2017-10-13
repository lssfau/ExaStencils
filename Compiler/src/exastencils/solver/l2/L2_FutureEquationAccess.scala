package exastencils.solver.l2

import exastencils.base.l2._
import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.solver.l3.L3_FutureEquationAccess
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureEquationAccess

case class L2_FutureEquationAccess(
    var name : String, var level : Int,
    var offset : Option[L2_ConstIndex] = None) extends L2_FutureKnowledgeAccess {

  override def prettyprint(out : PpStream) = {
    out << name << '@' << level
    if (offset.isDefined) out << '@' << offset.get
  }

  def progress = {
    Logger.warn(s"Trying to progress future equation access to $name on level $level")
    L3_FutureEquationAccess(name, level, L2_ProgressOption(offset)(_.progress))
  }

  def toEquationAccess = L2_EquationAccess(this)
}

/// L2_PrepareEquationAccesses

object L2_PrepareEquationAccesses extends DefaultStrategy("Prepare accesses to equations") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_EquationCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to equation ${ access.name }")
      }

      if (!L2_EquationCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      if (access.slot.isDefined) Logger.warn(s"Discarding meaningless slot access on ${ access.name }")
      if (access.dirAccess.isDefined) Logger.warn(s"Discarding meaningless direction access on ${ access.name }")
      if (access.arrayIndex.isDefined) Logger.warn(s"Discarding meaningless array access on ${ access.name }")

      L2_FutureEquationAccess(access.name, lvl, access.offset)
  })
}
