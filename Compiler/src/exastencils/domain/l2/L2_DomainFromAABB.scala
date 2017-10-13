package exastencils.domain.l2

import exastencils.base.l2._
import exastencils.domain.l3._
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_AABB

/// L2_DomainFromAABB

case class L2_DomainFromAABB(var name : String, aabb : L2_AABB) extends L2_Domain {
  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = L3_DomainFromAABB(name, aabb.progress)

  override def numDims = aabb.numDims
}

/// L2_DomainFromAABBDecl

case class L2_DomainFromAABBDecl(var name : String, lower : L2_Index, upper : L2_Index) extends L2_DomainDecl {
  override def prettyprint(out : PpStream) = out << "Domain " << name << "< " << lower << " to " << upper << " >"

  override def addToKnowledge() = {
    L2_DomainCollection.add(L2_DomainFromAABB(name, L2_AABB(lower.toExpressionIndex, upper.toExpressionIndex)))
  }
}
