package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._
import exastencils.logger.Logger

/// L2_VF_CellWidthAsVec

object L2_VF_CellWidthAsVec {
  def find(level : Int) = L2_VirtualField.findVirtualField(s"vf_cellWidth", level).asInstanceOf[L2_VF_CellWidthAsVec]
  def access(level : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level), index)
}

case class L2_VF_CellWidthAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L2_VF_CellWidthAsVec(level, domain)

  override def listPerDim = (0 until numDims).map(L2_VF_CellWidthPerDim.find(level, _) : L2_VirtualField).to[ListBuffer]

  override def progressImpl() = L3_VF_CellWidthAsVec(level, domain.getProgressedObj())
}

/// L2_VF_CellWidthPerDim

object L2_VF_CellWidthPerDim {
  def find(level : Int, dim : Int) = L2_VirtualField.findVirtualField(s"vf_cellWidth_$dim", level).asInstanceOf[L2_VF_CellWidthPerDim]
  def access(level : Int, dim : Int, index : L2_ExpressionIndex) = L2_VirtualFieldAccess(find(level, dim), index)
}

case class L2_VF_CellWidthPerDim(
    var level : Int,
    var domain : L2_Domain,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ L2_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ L2_Localization.dimToString(dim) }")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def createDuplicate() = L2_VF_CellWidthPerDim(level, domain, dim)

  override def resolve(index : L2_ExpressionIndex) = {
    if (Knowledge.grid_isUniform) {
      val levelIndex = level - Knowledge.minLevel
      dim match {
        case 0 => Knowledge.discr_hx(levelIndex)
        case 1 => Knowledge.discr_hy(levelIndex)
        case 2 => Knowledge.discr_hz(levelIndex)
      }

    } else if (Knowledge.grid_isAxisAligned) {
      L2_VF_NodePositionPerDim.access(level, dim, L2_GridUtil.offsetIndex(index, 1, dim)) - L2_VF_NodePositionPerDim.access(level, dim, index)

    } else {
      Logger.error("Currently unsupported")
    }
  }

  override def progressImpl() = L3_VF_CellWidthPerDim(level, domain.getProgressedObj(), dim)
}
