package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.logger.Logger

/// L3_VF_NodePositionAsVec

case class L3_VF_NodePositionAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L3_AtNode
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_NodePositionPerDim(level, domain, _) : L3_VirtualFieldPerDim).to[ListBuffer]
}

/// L3_VF_NodePositionPerDim

case class L3_VF_NodePositionPerDim(
    var level : Int,
    var domain : L3_Domain,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ L3_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ L3_Localization.dimToString(dim) }")
  override def localization = L3_AtNode
  override def resolutionPossible = false

  override def resolve(index : L3_ExpressionIndex) = Logger.error("Trying to resolve node position; unsupported")
  // FIXME: index(dim) * L3_VirtualFieldAccess(L3_VF_NodePositionPerDim(level, domain, dim), index) + L3_IV_FragmentPositionBegin(dim)
}
