package exastencils.domain.l2

import exastencils.baseExt.l2.L2_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l2.L2_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_LevelCollector

/// L2_FutureDomainAccess

case class L2_FutureDomainAccess(var name : String) extends L2_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name

  def progress = {
    Logger.warn(s"Trying to progress future domain access to $name")
    ??? // TODO
  }

  def toDomainAccess = L2_DomainAccess(this)
}

/// L2_PrepareDomainAccesses

object L2_PrepareDomainAccesses extends DefaultStrategy("Prepare accesses to domains") {
  val collector = new L2_LevelCollector
  this.register(collector)

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L2_UnresolvedAccess if L2_DomainCollection.existsDecl(access.name) =>
      if (!L2_DomainCollection.existsDecl(access.name))
        Logger.warn(s"Trying to access ${ access.name }")

      if (access.level.isDefined)
        Logger.warn(s"Ignoring invalid level specification in access to domain ${ access.name }")

      L2_FutureDomainAccess(access.name)
  })
}

