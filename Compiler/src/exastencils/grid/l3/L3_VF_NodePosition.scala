package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._
import exastencils.logger.Logger

/// L3_VF_NodePositionAsVec

object L3_VF_NodePositionAsVec {
  def find(level : Int) = L3_VirtualField.findVirtualField(s"vf_nodePosition", level)
}

case class L3_VF_NodePositionAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L3_AtNode
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_NodePositionPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = L4_VF_NodePositionAsVec(level, domain.getProgressedObj())
}

/// L3_VF_NodePositionPerDim

object L3_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level)
}

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

  override def progressImpl() = L4_VF_NodePositionPerDim(level, domain.getProgressedObj(), dim)
}
