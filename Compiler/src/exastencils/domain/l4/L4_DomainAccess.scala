package exastencils.domain.l4

import exastencils.datastructures._
import exastencils.knowledge.l4._
import exastencils.logger.Logger
import exastencils.prettyprinting.PpStream

/// L4_DomainAccess

object L4_DomainAccess {
  def apply(name : String) =
    new L4_DomainAccess(L4_DomainCollection.getByIdentifier(name).get)

  def apply(access : L4_FutureDomainAccess) =
    new L4_DomainAccess(L4_DomainCollection.getByIdentifier(access.name).get)
}

case class L4_DomainAccess(var target : L4_Domain) extends L4_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = Logger.error(s"Trying to progress access to domain ${ target.name } - unsupported")
}

/// L4_ResolveDomainAccesses

object L4_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L4_FutureDomainAccess if L4_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
