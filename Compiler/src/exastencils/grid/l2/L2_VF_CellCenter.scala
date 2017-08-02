package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._

/// L2_VF_CellCenterAsVec

object L2_VF_CellCenterAsVec {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_cellCenter", level)
}

case class L2_VF_CellCenterAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L2_VF_CellCenterPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = L3_VF_CellCenterAsVec(level, domain.getProgressedObj())
}

/// L2_VF_CellCenterPerDim

object L2_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level)
}

case class L2_VF_CellCenterPerDim(
    var level : Int,
    var domain : L2_Domain,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ L2_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ L2_Localization.dimToString(dim) }")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L2_ExpressionIndex) = {
    // nodePos + 0.5 cellWidth
    L2_VirtualFieldAccess(L2_VF_NodePositionPerDim.find(level, dim), index) +
      0.5 * L2_VirtualFieldAccess(L2_VF_CellWidthPerDim.find(level, dim), index)
  }

  override def progressImpl() = L3_VF_CellCenterPerDim(level, domain.getProgressedObj(), dim)
}
