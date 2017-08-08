package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VF_CellVolume
import exastencils.logger.Logger

/// L4_VF_CellVolume

object L4_VF_CellVolume {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_cellVolume", level)
  def access(level : Int, index : L4_ExpressionIndex) = L4_VirtualFieldAccess(find(level), index)
}

case class L4_VF_CellVolume(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L4_ExpressionIndex) = {
    if (Knowledge.grid_isAxisAligned) // includes uniform grids
      (0 until domain.numDims).map(dim => L4_VF_CellWidthPerDim.access(level, dim, Duplicate(index)) : L4_Expression).reduce(_ * _)
    else
      Logger.error("Currently unsupported")
  }

  override def progressImpl() = IR_VF_CellVolume(level, domain.getProgressedObj())
}
