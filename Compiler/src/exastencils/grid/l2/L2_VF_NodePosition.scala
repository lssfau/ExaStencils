package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L2_VF_NodePositionAsVec

object L2_VF_NodePositionAsVec {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_nodePosition", level)
  def access(level : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level), index)
}

case class L2_VF_NodePositionAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L2_AtNode
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L2_VF_NodePositionPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = L3_VF_NodePositionAsVec(level, domain.getProgressedObj())
}

/// L2_VF_NodePositionPerDim

object L2_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level)
  def access(level : Int, dim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, dim), index)
}

case class L2_VF_NodePositionPerDim(
    var level : Int,
    var domain : L2_Domain,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ L2_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ L2_Localization.dimToString(dim) }")
  override def localization = L2_AtNode
  override def resolutionPossible = false

  override def resolve(index : L2_ExpressionIndex) = Logger.error("Trying to resolve node position; unsupported")

  override def progressImpl() = L3_VF_NodePositionPerDim(level, domain.getProgressedObj(), dim)
}
