package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.config.Knowledge
import exastencils.domain.l3.L3_Domain
import exastencils.grid.l4._
import exastencils.logger.Logger

/// L3_VF_CellWidthAsVec

object L3_VF_CellWidthAsVec {
  def find(level : Int) = L3_VirtualField.findVirtualField(s"vf_cellWidth", level).asInstanceOf[L3_VF_CellWidthAsVec]
  def access(level : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level), index)
}

case class L3_VF_CellWidthAsVec(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L3_VF_CellWidthAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L3_VF_CellWidthPerDim.find(level, _) : L3_VirtualField).to[ListBuffer]

  override def progressImpl() = L4_VF_CellWidthAsVec(level, domain.getProgressedObj())
}

/// L3_VF_CellWidthPerDim

object L3_VF_CellWidthPerDim {
  def find(level : Int, dim : Int) = L3_VirtualField.findVirtualField(s"vf_cellWidth_$dim", level).asInstanceOf[L3_VF_CellWidthPerDim]
  def access(level : Int, dim : Int, index : L3_ExpressionIndex) = L3_VirtualFieldAccess(find(level, dim), index)
}

case class L3_VF_CellWidthPerDim(
    var level : Int,
    var domain : L3_Domain,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ L3_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ L3_Localization.dimToString(dim) }")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L3_VF_CellWidthPerDim(level, domain, dim)

  override def resolve(index : L3_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      val levelIndex = level - Knowledge.minLevel
      dim match {
        case 0 => Knowledge.discr_hx(levelIndex)
        case 1 => Knowledge.discr_hy(levelIndex)
        case 2 => Knowledge.discr_hz(levelIndex)
      }

    } else if (Knowledge.grid_isAxisAligned) {
      L3_VF_NodePositionPerDim.access(level, dim, L3_GridUtil.offsetIndex(index, 1, dim)) - L3_VF_NodePositionPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = L4_VF_CellWidthPerDim(level, domain.getProgressedObj(), dim)
}
