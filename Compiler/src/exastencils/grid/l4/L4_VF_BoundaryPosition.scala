package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir._
import exastencils.logger.Logger

/// L4_VF_BoundaryPositionAsVec

object L4_VF_BoundaryPositionAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_boundaryPosition", level)
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_BoundaryPositionAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_boundaryPosition"
  override def knownAliases = ListBuffer("vf_boundaryCoordinateAsVec", "vf_boundaryCoordAsVec", "vf_boundaryPositionAsVec", "vf_boundaryPosAsVec", "vf_boundaryPos")
  override def localization = L4_AtBoundary
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L4_VF_BoundaryPositionPerDim.find(level, _)).to[ListBuffer]
  override def progressImpl() = IR_VF_BoundaryPositionAsVec(level, domain.getProgressedObj())
}

/// L4_VF_BoundaryPositionPerDim

object L4_VF_BoundaryPositionPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_boundaryPosition_$dim", level)
  def access(level : Int, dim : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level, dim), index)
}

case class L4_VF_BoundaryPositionPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_boundaryPosition_$dim"
  override def knownAliases = {
    ListBuffer(dim.toString, L4_Localization.dimToString(dim)).flatMap(postfix =>
      ListBuffer(s"vf_boundaryPos_$postfix", s"vf_boundaryCoordinate_$postfix", s"vf_boundaryCoord_$postfix") :+
        s"vf_boundaryPosition_${ L4_Localization.dimToString(dim) }"
    )
  }
  override def localization = L4_AtBoundary
  override def resolutionPossible = false

  override def resolve(index : L4_ExpressionIndex) = Logger.error("Trying to resolve boundary position; unsupported")
  override def progressImpl() = IR_VF_BoundaryPositionPerDim(level, domain.getProgressedObj(), dim)
}
