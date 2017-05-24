package exastencils.domain.l3

import exastencils.base.l3._
import exastencils.domain.l4._
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_AABB

/// L3_DomainFromAABB

case class L3_DomainFromAABB(var name : String, aabb : L3_AABB) extends L3_Domain {
  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = L4_DomainFromAABB(name, aabb.progress)
}

/// L3_DomainFromAABBDecl

case class L3_DomainFromAABBDecl(var name : String, lower : L3_Index, upper : L3_Index) extends L3_DomainDecl {
  override def prettyprint(out : PpStream) = out << "Domain " << name << "< " << lower << " to " << upper << " >"

  override def addToKnowledge() = {
    L3_DomainCollection.add(L3_DomainFromAABB(name, L3_AABB(lower.toExpressionIndex, upper.toExpressionIndex)))
  }
}
