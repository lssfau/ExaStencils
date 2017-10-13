package exastencils.domain.l3

import exastencils.datastructures._
import exastencils.domain.l4.L4_DomainAccess
import exastencils.knowledge.l3._
import exastencils.prettyprinting.PpStream

/// L3_DomainAccess

object L3_DomainAccess {
  def apply(name : String) =
    new L3_DomainAccess(L3_DomainCollection.getByIdentifier(name).get)

  def apply(access : L3_FutureDomainAccess) =
    new L3_DomainAccess(L3_DomainCollection.getByIdentifier(access.name).get)
}

case class L3_DomainAccess(var target : L3_Domain) extends L3_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = L4_DomainAccess(target.getProgressedObj())
}

/// L3_ResolveDomainAccesses

object L3_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L3_FutureDomainAccess if L3_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
