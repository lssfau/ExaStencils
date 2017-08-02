package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3.L3_VF_StagCellVolume

/// L2_VF_StagCellVolume

object L2_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L2_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
}

case class L2_VF_StagCellVolume(
    var level : Int,
    var domain : L2_Domain,
    var stagDim : Int
) extends L2_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L2_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L2_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      L2_VirtualFieldAccess(L2_VF_StagCellWidthPerDim.find(level, stagDim, dim), index) : L2_Expression
    ).reduce(_ * _)
  }

  override def progressImpl() = L3_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
