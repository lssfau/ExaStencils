package exastencils.domain.l4

import exastencils.domain.ir._
import exastencils.prettyprinting.PpStream
import exastencils.util.l4.L4_AABB

/// L4_DomainFromAABB

case class L4_DomainFromAABB(var name : String, aabb : L4_AABB) extends L4_Domain {
  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = IR_DomainFromAABB(name, aabb.progress)

  override def numDims = aabb.numDims
}

/// L4_DomainFromAABBDecl

case class L4_DomainFromAABBDecl(var name : String, lower : Array[Double], upper : Array[Double]) extends L4_DomainDecl {
  override def prettyprint(out : PpStream) = out << "Domain " << name << "< " << lower << " to " << upper << " >"

  override def addToKnowledge() = {
    L4_DomainCollection.add(L4_DomainFromAABB(name, L4_AABB(lower, upper)))
  }
}
