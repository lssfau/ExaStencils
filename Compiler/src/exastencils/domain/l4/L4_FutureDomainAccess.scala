package exastencils.domain.l4

import exastencils.baseExt.l4.L4_UnresolvedAccess
import exastencils.datastructures._
import exastencils.knowledge.l4.L4_FutureKnowledgeAccess
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_LevelCollector

/// L4_FutureDomainAccess

case class L4_FutureDomainAccess(var name : String) extends L4_FutureKnowledgeAccess {
  override def prettyprint(out : PpStream) = out << name
  override def progress = Logger.error(s"Trying to progress future domain access to $name")
  def toDomainAccess = L4_DomainAccess(this)
}

/// L4_PrepareDomainAccesses

object L4_PrepareDomainAccesses extends DefaultStrategy("Prepare accesses to domains") {
  val collector = new L4_LevelCollector
  this.register(collector)
  this.onBefore = () => this.resetCollectors()

  this += new Transformation("Resolve applicable unresolved accesses", {
    case access : L4_UnresolvedAccess if L4_DomainCollection.existsDecl(access.name) =>
      if (!L4_DomainCollection.existsDecl(access.name))
        Logger.warn(s"Trying to access ${ access.name }")

      if (access.level.isDefined)
        Logger.warn(s"Ignoring invalid level specification in access to domain ${ access.name }")

      L4_FutureDomainAccess(access.name)
  })
}

