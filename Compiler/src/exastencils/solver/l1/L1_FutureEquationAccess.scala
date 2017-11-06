package exastencils.solver.l1

import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.solver.l2.L2_FutureEquationAccess
import exastencils.util.l1.L1_LevelCollector

/// L1_FutureEquationAccess

case class L1_FutureEquationAccess(var name : String, var level : Int) extends L1_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future equation access to $name on level $level")
    L2_FutureEquationAccess(name, level, None)
  }

  def toEquationAccess = L1_EquationAccess(this)
}

/// L1_PrepareEquationAccesses

object L1_PrepareEquationAccesses extends DefaultStrategy("Prepare accesses to equations") {
  val collector = new L1_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L1_UnresolvedAccess if L1_EquationCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to equation ${ access.name }")
      }

      if (!L1_EquationCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L1_FutureEquationAccess(access.name, lvl)
  })
}
