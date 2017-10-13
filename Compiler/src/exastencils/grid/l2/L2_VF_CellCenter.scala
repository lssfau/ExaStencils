package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.core.Duplicate
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._

/// L2_VF_CellCenterAsVec

object L2_VF_CellCenterAsVec {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_cellCenter", level).asInstanceOf[L2_VF_CellCenterAsVec]
  def access(level : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level), index)
}

case class L2_VF_CellCenterAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_cellCenter"
  override def knownAliases = ListBuffer("vf_cellCenterAsVec", "vf_cellCenAsVec", "vf_cellCen")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L2_VF_CellCenterPerDim.find(level, _) : L2_VirtualField).to[ListBuffer]

  override def progressImpl() = L3_VF_CellCenterAsVec(level, domain.getProgressedObj())
}

/// L2_VF_CellCenterPerDim

object L2_VF_CellCenterPerDim {
  def find(level : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_cellCenter_$dim", level).asInstanceOf[L2_VF_CellCenterPerDim]
  def access(level : Int, dim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, dim), index)
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
    if (Knowledge.grid_isUniform) {
      // nodePos + 0.5 * cellWidth
      L2_VF_NodePositionPerDim.access(level, dim, index) + 0.5 * L2_VF_CellWidthPerDim.access(level, dim, index)

    } else if (Knowledge.grid_isAxisAligned) {
      // interpolation of two node positions suffices
      0.5 * (L2_VF_NodePositionPerDim.access(level, dim, index)
        + L2_VF_NodePositionPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, 1, dim)))

    } else {
      // interpolation of the current cell's corner positions

      var accesses = ListBuffer[L2_VirtualFieldAccess](L2_VF_NodePositionPerDim.access(level, dim, index))

      for (d <- 0 until numDims)
        accesses = accesses.flatMap(base => {
          val left = Duplicate(base)
          val right = Duplicate(base)
          right.index = L2_GridUtil.offsetIndex(right.index, 1, d)

          ListBuffer(left, right)
        })

      (1.0 / accesses.length) * L2_Addition(accesses.map(_.asInstanceOf[L2_Expression]))
    }
  }

  override def progressImpl() = L3_VF_CellCenterPerDim(level, domain.getProgressedObj(), dim)
}
