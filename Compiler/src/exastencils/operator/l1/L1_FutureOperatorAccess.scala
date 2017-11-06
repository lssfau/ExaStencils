package exastencils.operator.l1

import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.operator.l2.L2_FutureStencilAccess
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_LevelCollector

/// L1_FutureOperatorAccess

case class L1_FutureOperatorAccess(var name : String, var level : Int) extends L1_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name << '@' << level

  def progress = {
    Logger.warn(s"Trying to progress future operator access to $name on level $level")
    L2_FutureStencilAccess(name, level, None, None)
  }

  def toOperatorAccess = L1_OperatorAccess(this)
}

/// L1_PrepareOperatorAccesses

object L1_PrepareOperatorAccesses extends DefaultStrategy("Prepare accesses to operators") {
  val collector = new L1_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L1_UnresolvedAccess if L1_OperatorCollection.existsDecl(access.name) =>
      val lvl = {
        if (access.level.isDefined) access.level.get.resolveLevel
        else if (collector.inLevelScope) collector.getCurrentLevel
        else Logger.error(s"Missing level for access to operator ${ access.name }")
      }

      if (!L1_OperatorCollection.existsDecl(access.name, lvl))
        Logger.warn(s"Trying to access ${ access.name } on invalid level $lvl")

      L1_FutureOperatorAccess(access.name, lvl)
  })
}
