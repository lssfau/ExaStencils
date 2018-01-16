package exastencils.domain.l1

import exastencils.domain.l2._
import exastencils.prettyprinting.PpStream
import exastencils.util.l1.L1_AABB

/// L1_DomainFromAABB

case class L1_DomainFromAABB(override var name : String, aabb : L1_AABB) extends L1_Domain {
  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = L2_DomainFromAABB(name, aabb.progress)

  override def numDims = aabb.numDims
}

/// L1_DomainFromAABBDecl

case class L1_DomainFromAABBDecl(var name : String, lower : Array[Double], upper : Array[Double]) extends L1_DomainDecl {
  override def prettyprint(out : PpStream) = {
    out << "Domain " << name << " = [" << lower.mkString(", ") << "] to [" << upper.mkString(", ") << "]"
  }

  override def addToKnowledge() = {
    L1_DomainCollection.add(L1_DomainFromAABB(name, L1_AABB(lower, upper)))
  }
}
