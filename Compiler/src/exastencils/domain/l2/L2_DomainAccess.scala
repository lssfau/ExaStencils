package exastencils.domain.l2

import exastencils.datastructures._
import exastencils.domain.l3.L3_DomainAccess
import exastencils.knowledge.l2._
import exastencils.prettyprinting.PpStream

/// L2_DomainAccess

object L2_DomainAccess {
  def apply(name : String) =
    new L2_DomainAccess(L2_DomainCollection.getByIdentifier(name).get)

  def apply(access : L2_FutureDomainAccess) =
    new L2_DomainAccess(L2_DomainCollection.getByIdentifier(access.name).get)
}

case class L2_DomainAccess(var target : L2_Domain) extends L2_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = L3_DomainAccess(target.getProgressedObj())
}

/// L2_ResolveDomainAccesses

object L2_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L2_FutureDomainAccess if L2_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
