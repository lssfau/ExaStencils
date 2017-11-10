package exastencils.domain.l2

import exastencils.config.Knowledge
import exastencils.domain.l3._
import exastencils.prettyprinting.PpStream
import exastencils.util.l2.L2_AABB

/// L2_DomainFromAABB

case class L2_DomainFromAABB(var name : String, aabb : L2_AABB) extends L2_Domain {
  /// HACK: make this information accessible in some other way
  if (Knowledge.domain_rect_generate && "global" == name) {
    Knowledge.discr_hx = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
      level => aabb.width(0) / (Knowledge.domain_rect_numFragsTotal_x * Knowledge.domain_fragmentLength_x * (1 << level)))
    if (Knowledge.dimensionality > 1)
      Knowledge.discr_hy = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => aabb.width(1) / (Knowledge.domain_rect_numFragsTotal_y * Knowledge.domain_fragmentLength_y * (1 << level)))
    if (Knowledge.dimensionality > 2)
      Knowledge.discr_hz = (Knowledge.minLevel to Knowledge.maxLevel).toArray.map(
        level => aabb.width(2) / (Knowledge.domain_rect_numFragsTotal_z * Knowledge.domain_fragmentLength_z * (1 << level)))
  }

  override def prettyprintDecl(out : PpStream) = out << "Domain " << name << "< " << aabb << " >"
  override def progressImpl() = L3_DomainFromAABB(name, aabb.progress)

  override def numDims = aabb.numDims
}

/// L2_DomainFromAABBDecl

case class L2_DomainFromAABBDecl(var name : String, lower : Array[Double], upper : Array[Double]) extends L2_DomainDecl {
  override def prettyprint(out : PpStream) = out << "Domain " << name << "< " << lower << " to " << upper << " >"

  override def addToKnowledge() = {
    L2_DomainCollection.add(L2_DomainFromAABB(name, L2_AABB(lower, upper)))
  }
}
