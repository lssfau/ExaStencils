package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l3.L3_Domain
import exastencils.logger.Logger
import exastencils.grid.l4.L4_VF_CellVolume

/// L3_VF_CellVolume

object L3_VF_CellVolume {
  def find(level : Int) = L3_VirtualField.findVirtualField(s"vf_cellVolume", level).asInstanceOf[L3_VF_CellVolume]
  def access(level : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level), index)
}

case class L3_VF_CellVolume(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L3_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) : L3_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = L4_VF_CellVolume(level, domain.getProgressedObj())
}
