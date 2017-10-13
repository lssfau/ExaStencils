package exastencils.domain.l3

import exastencils.baseExt.l3.L3_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l3.L3_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_LevelCollector

/// L3_FutureDomainAccess

case class L3_FutureDomainAccess(var name : String) extends L3_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name

  def progress = {
    Logger.warn(s"Trying to progress future domain access to $name")
    ??? // TODO
  }

  def toDomainAccess = L3_DomainAccess(this)
}

/// L3_PrepareDomainAccesses

object L3_PrepareDomainAccesses extends DefaultStrategy("Prepare accesses to domains") {
  val collector = new L3_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L3_UnresolvedAccess if L3_DomainCollection.existsDecl(access.name) =>
      if (!L3_DomainCollection.existsDecl(access.name))
        Logger.warn(s"Trying to access ${ access.name }")

      if (access.level.isDefined)
        Logger.warn(s"Ignoring invalid level specification in access to domain ${ access.name }")

      L3_FutureDomainAccess(access.name)
  })
}

