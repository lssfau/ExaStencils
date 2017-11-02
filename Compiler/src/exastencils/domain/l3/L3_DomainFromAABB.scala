package exastencils.domain.l3

import exastencils.domain.l4._
import exastencils.prettyprinting.PpStream
import exastencils.util.l3.L3_AABB

/// L3_DomainFromAABB

case class L3_DomainFromAABB(var name : String, aabb : L3_AABB) extends L3_Domain {
  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = L4_DomainFromAABB(name, aabb.progress)

  override def numDims = aabb.numDims
}

/// L3_DomainFromAABBDecl

case class L3_DomainFromAABBDecl(var name : String, lower : Array[Double], upper : Array[Double]) extends L3_DomainDecl {
  override def prettyprint(out : PpStream) = out << "Domain " << name << "< " << lower << " to " << upper << " >"

  override def addToKnowledge() = {
    L3_DomainCollection.add(L3_DomainFromAABB(name, L3_AABB(lower, upper)))
  }
}
