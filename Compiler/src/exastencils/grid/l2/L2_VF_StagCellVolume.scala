package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.logger.Logger
import exastencils.grid.l3.L3_VF_StagCellVolume

/// L2_VF_StagCellVolume

object L2_VF_StagCellVolume {
  def find(level : Int, stagDim : Int) = L2_VirtualField.findVirtualField(s"vf_stag_${ stagDim }_cellVolume", level)
  def access(level : Int, stagDim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, stagDim), index)
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
    if (!Knowledge.grid_isStaggered) Logger.error("Trying to resolve a staggered quantity on a non-staggered grid; unsupported")

    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L2_VF_StagCellWidthPerDim.access(level, stagDim, dim, index) : L2_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = L3_VF_StagCellVolume(level, domain.getProgressedObj(), stagDim)
}
