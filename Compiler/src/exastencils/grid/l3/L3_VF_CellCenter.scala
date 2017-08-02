package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain

/// L3_VF_CellCenterAsVec

case class L3_VF_CellCenterAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_CellCenterPerDim(level, domain, _) : L3_VirtualFieldPerDim).to[ListBuffer]
}

/// L3_VF_CellCenterPerDim

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
    L3_VirtualFieldAccess(L3_VF_NodePositionPerDim(level, domain, dim), index) +
      0.5 * L3_VirtualFieldAccess(L3_VF_CellWidthPerDim(level, domain, dim), index)
  }
}
