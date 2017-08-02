package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4.L4_VirtualField
import exastencils.logger.Logger

/// L3_VF_BoundaryPositionAsVec

case class L3_VF_BoundaryPositionAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_boundaryPosition"
  override def knownAliases = ListBuffer("vf_boundaryCoordinateAsVec", "vf_boundaryCoordAsVec", "vf_boundaryPositionAsVec", "vf_boundaryPosAsVec", "vf_boundaryPos")
  override def localization = L3_AtBoundary
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_BoundaryPositionPerDim(level, domain, _) : L3_VirtualFieldPerDim).to[ListBuffer]
}

/// L3_VF_BoundaryPositionPerDim

case class L3_VF_BoundaryPositionPerDim(
    var level : Int,
    var domain : L3_Domain,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_boundaryPosition_$dim"
  override def knownAliases = {
    ListBuffer(dim.toString, L3_Localization.dimToString(dim)).flatMap(postfix =>
      ListBuffer(s"vf_boundaryPos_$postfix", s"vf_boundaryCoordinate_$postfix", s"vf_boundaryCoord_$postfix") :+
        s"vf_boundaryPosition_${ L3_Localization.dimToString(dim) }"
    )
  }
  override def localization = L3_AtBoundary
  override def resolutionPossible = false

  override def resolve(index : L3_ExpressionIndex) = Logger.error("Trying to resolve boundary position; unsupported")

  override def progressImpl() = /* FIXME */ L4_VirtualField("vf_boundaryCoord_" + L3_Localization.dimToString(dim), level, domain.getProgressedObj(), datatype.progress, localization.name)
}
