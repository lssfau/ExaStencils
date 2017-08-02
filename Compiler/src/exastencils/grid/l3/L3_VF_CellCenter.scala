package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._

/// L3_VF_CellCenterAsVec

object L3_VF_CellCenterAsVec {
  def find(level : Int) = L3_VirtualField.findVirtualField(s"vf_cellCenter", level)
}

case class L3_VF_CellCenterAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_CellCenterPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = L4_VF_CellCenterAsVec(level, domain.getProgressedObj())
}

/// L3_VF_CellCenterPerDim

object L3_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level)
}

case class L3_VF_CellCenterPerDim(
    var level : Int,
    var domain : L3_Domain,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ L3_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ L3_Localization.dimToString(dim) }")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    // nodePos + 0.5 cellWidth
    L3_VirtualFieldAccess(L3_VF_NodePositionPerDim.find(level, dim), index) +
      0.5 * L3_VirtualFieldAccess(L3_VF_CellWidthPerDim.find(level, dim), index)
  }

  override def progressImpl() = L4_VF_CellCenterPerDim(level, domain.getProgressedObj(), dim)
}
