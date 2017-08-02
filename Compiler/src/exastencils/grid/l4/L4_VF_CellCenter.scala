package exastencils.grid.l4

import scala.collection.mutable.ListBuffer

import exastencils.base.l4.L4_ImplicitConversion._
import exastencils.base.l4._
import exastencils.domain.l4.L4_Domain
import exastencils.grid.ir._

/// L4_VF_CellCenterAsVec

object L4_VF_CellCenterAsVec {
  def find(level : Int) = L4_VirtualField.findVirtualField(s"vf_cellCenter", level)
}

case class L4_VF_CellCenterAsVec(
    var level : Int,
    var domain : L4_Domain
) extends L4_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L4_VF_CellCenterPerDim.find(level, _)).to[ListBuffer]

  override def progressImpl() = IR_VF_CellCenterAsVec(level, domain.getProgressedObj())
}

/// L4_VF_CellCenterPerDim

object L4_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = L4_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level)
}

case class L4_VF_CellCenterPerDim(
    var level : Int,
    var domain : L4_Domain,
    var dim : Int
) extends L4_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ L4_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ L4_Localization.dimToString(dim) }")
  override def localization = L4_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L4_ExpressionIndex) = {
    // nodePos + 0.5 cellWidth
    L4_VirtualFieldAccess(L4_VF_NodePositionPerDim.find(level, dim), index) +
      0.5 * L4_VirtualFieldAccess(L4_VF_CellWidthPerDim.find(level, dim), index)
  }

  override def progressImpl() = IR_VF_CellCenterPerDim(level, domain.getProgressedObj(), dim)
}
