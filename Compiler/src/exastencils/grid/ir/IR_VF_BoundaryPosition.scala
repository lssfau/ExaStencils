package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain
import exastencils.logger.Logger

/// IR_VF_BoundaryPositionAsVec

object IR_VF_BoundaryPositionAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_boundaryPosition", level)
}

case class IR_VF_BoundaryPositionAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_boundaryPosition"
  override def knownAliases = ListBuffer("vf_boundaryCoordinateAsVec", "vf_boundaryCoordAsVec", "vf_boundaryPositionAsVec", "vf_boundaryPosAsVec", "vf_boundaryPos")
  override def localization = IR_AtBoundary
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(IR_VF_BoundaryPositionPerDim.find(level, _)).to[ListBuffer]
}

/// IR_VF_BoundaryPositionPerDim

object IR_VF_BoundaryPositionPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_boundaryPosition_$dim", level)
}

case class IR_VF_BoundaryPositionPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_boundaryPosition_$dim"
  override def knownAliases = {
    ListBuffer(dim.toString, IR_Localization.dimToString(dim)).flatMap(postfix =>
      ListBuffer(s"vf_boundaryPos_$postfix", s"vf_boundaryCoordinate_$postfix", s"vf_boundaryCoord_$postfix") :+
        s"vf_boundaryPosition_${ IR_Localization.dimToString(dim) }"
    )
  }
  override def localization = IR_AtBoundary
  override def resolutionPossible = false

  override def resolve(index : IR_ExpressionIndex) = Logger.error("Trying to resolve boundary position; unsupported")
}
