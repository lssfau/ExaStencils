package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4.L4_VF_StagCellVolume

/// L3_VF_StagCellVolume

object L3_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L3_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
}

case class L3_VF_StagCellVolume(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      L3_VirtualFieldAccess(L3_VF_StagCellWidthPerDim.find(level, stagDim, dim), index) : L3_Expression
    ).reduce(_ * _)
  }

  override def progressImpl() = L4_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
