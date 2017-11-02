package exastencils.domain.l1

import exastencils.baseExt.l1.L1_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l1.L1_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_LevelCollector

/// L1_FutureDomainAccess

case class L1_FutureDomainAccess(var name : String) extends L1_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name

  def progress = {
    Logger.warn(s"Trying to progress future domain access to $name")
    ??? // TODO
  }

  def toDomainAccess = L1_DomainAccess(this)
}

/// L1_PrepareDomainAccesses

object L1_PrepareDomainAccesses extends DefaultStrategy("Prepare accesses to domains") {
  val collector = new L1_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L1_UnresolvedAccess if L1_DomainCollection.existsDecl(access.name) =>
      if (!L1_DomainCollection.existsDecl(access.name))
        Logger.warn(s"Trying to access ${ access.name }")

      if (access.level.isDefined)
        Logger.warn(s"Ignoring invalid level specification in access to domain ${ access.name }")

      L1_FutureDomainAccess(access.name)
  })
}

