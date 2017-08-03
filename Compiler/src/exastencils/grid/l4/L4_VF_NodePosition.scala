package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir._
import exastencils.logger.Logger

/// L4_VF_NodePositionAsVec

object L4_VF_NodePositionAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_nodePosition", level)
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_NodePositionAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_nodePosition"
  override def knownAliases = ListBuffer("vf_nodePositionAsVec", "vf_nodePosAsVec", "vf_nodePos")
  override def localization = L4_AtNode
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L4_VF_NodePositionPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = IR_VF_NodePositionAsVec(level, domain.getProgressedObj())
}

/// L4_VF_NodePositionPerDim

object L4_VF_NodePositionPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_nodePosition_$dim", level)
  def access(level : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, dim), index)
}

case class L4_VF_NodePositionPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_nodePosition_$dim"
  override def knownAliases = ListBuffer(s"vf_nodePosition_${ L4_Localization.dimToString(dim) }", s"vf_nodePos_$dim", s"vf_nodePos_${ L4_Localization.dimToString(dim) }")
  override def localization = L4_AtNode
  override def resolutionPossible = false

  override def resolve(index : L4_ExpressionIndex) = Logger.error("Trying to resolve node position; unsupported")

  override def progressImpl() = IR_VF_NodePositionPerDim(level, domain.getProgressedObj(), dim)
}
