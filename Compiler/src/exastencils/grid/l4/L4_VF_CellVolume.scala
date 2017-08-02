package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir.IR_VF_CellVolume

/// L4_VF_CellVolume

object L4_VF_CellVolume {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_cellVolume", level)
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
    (0 until domain.numDims).map(dim =>
      L4_VirtualFieldAccess(L4_VF_CellWidthPerDim.find(level, dim), index) : L4_Expression
    ).reduce(_ * _)
  }

  override def progressImpl() = IR_VF_CellVolume(level, domain.getProgressedObj())
}
