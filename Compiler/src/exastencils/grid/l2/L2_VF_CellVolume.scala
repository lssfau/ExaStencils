package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3.L3_VF_CellVolume
import exastencils.logger.Logger

/// L2_VF_CellVolume

object L2_VF_CellVolume {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_cellVolume", level).asInstanceOf[L2_VF_CellVolume]
  def access(level : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level), index)
}

case class L2_VF_CellVolume(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L2_ExpressionIndex) = {
    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L2_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) : L2_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = L3_VF_CellVolume(level, domain.getProgressedObj())
}
