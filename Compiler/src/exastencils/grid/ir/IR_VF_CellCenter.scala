package exastencils.grid.ir

import scala.collection.mutable.ListBuffer

import exastencils.base.ir.IR_ImplicitConversion._
import exastencils.base.ir._
import exastencils.domain.ir.IR_Domain

/// IR_VF_CellCenterAsVec

object IR_VF_CellCenterAsVec {
  def find(level : Int) = IR_VirtualField.findVirtualField(s"vf_cellCenter", level)
}

case class IR_VF_CellCenterAsVec(
    var level : Int,
    var domain : IR_Domain
) extends IR_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(IR_VF_CellCenterPerDim.find(level, _)).to[ListBuffer]
}

/// IR_VF_CellCenterPerDim

object IR_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = IR_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level)
}

case class IR_VF_CellCenterPerDim(
    var level : Int,
    var domain : IR_Domain,
    var dim : Int
) extends IR_VirtualFieldPerDim {

  override def name = s"vf_cellCenter_$dim"
  override def knownAliases = ListBuffer(s"vf_cellCenter_${ IR_Localization.dimToString(dim) }", s"vf_cellCen_$dim", s"vf_cellCen_${ IR_Localization.dimToString(dim) }")
  override def localization = IR_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : IR_ExpressionIndex) = {
    // nodePos + 0.5 cellWidth
    IR_VirtualFieldAccess(IR_VF_NodePositionPerDim.find(level, dim), index) +
      0.5 * IR_VirtualFieldAccess(IR_VF_CellWidthPerDim.find(level, dim), index)
  }
}
