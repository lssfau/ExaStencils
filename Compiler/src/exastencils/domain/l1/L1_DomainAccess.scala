package exastencils.domain.l1

import exastencils.datastructures._
import exastencils.domain.l2.L2_DomainAccess
import exastencils.knowledge.l1._
import exastencils.prettyprinting.PpStream

/// L1_DomainAccess

object L1_DomainAccess {
  def apply(name : String) =
    new L1_DomainAccess(L1_DomainCollection.getByIdentifier(name).get)

  def apply(access : L1_FutureDomainAccess) =
    new L1_DomainAccess(L1_DomainCollection.getByIdentifier(access.name).get)
}

case class L1_DomainAccess(var target : L1_Domain) extends L1_KnowledgeAccess {
  override def prettyprint(out : PpStream) = out << target.name
  override def progress = L2_DomainAccess(target.getProgressedObj())
}

/// L1_ResolveDomainAccesses

object L1_ResolveDomainAccesses extends DefaultStrategy("Resolve accesses to domains") {
  this += new Transformation("Resolve applicable future accesses", {
    // check if declaration has already been processed and promote access if possible
    case access : L1_FutureDomainAccess if L1_DomainCollection.exists(access.name) =>
      access.toDomainAccess
  })
}
