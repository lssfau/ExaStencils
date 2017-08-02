package exastencils.grid.l2

import scala.collection.mutable.ListBuffer

import exastencils.base.l2.L2_ImplicitConversion._
import exastencils.base.l2._
import exastencils.config.Knowledge
import exastencils.domain.l2.L2_Domain
import exastencils.grid.l3._

/// L2_VF_CellWidthAsVec

case class L2_VF_CellWidthAsVec(
    var level : Int,
    var domain : L2_Domain
) extends L2_VirtualFieldWithVec {

  override def name = "vf_cellWidth"
  override def knownAliases = ListBuffer("vf_cellWidthAsVec", "vf_gridWidthAsVec", "vf_gridWidth")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L2_VF_CellWidthPerDim(level, domain, _) : L2_VirtualFieldPerDim).to[ListBuffer]

  override def progressImpl() = L3_VF_CellWidthAsVec(level, domain.getProgressedObj())
}

/// L2_VF_CellWidthPerDim

case class L2_VF_CellWidthPerDim(
    var level : Int,
    var domain : L2_Domain,
    var dim : Int
) extends L2_VirtualFieldPerDim {

  override def name = s"vf_cellWidth_$dim"
  override def knownAliases = ListBuffer(s"vf_cellWidth_${ L2_Localization.dimToString(dim) }", s"vf_gridWidth_$dim", s"vf_gridWidth_${ L2_Localization.dimToString(dim) }")
  override def localization = L2_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L2_ExpressionIndex) = {
    val levelIndex = level - Knowledge.minLevel
    dim match {
      case 0 => Knowledge.discr_hx(levelIndex)
      case 1 => Knowledge.discr_hy(levelIndex)
      case 2 => Knowledge.discr_hz(levelIndex)
    }
  }

  override def progressImpl() = L3_VF_CellWidthPerDim(level, domain.getProgressedObj(), dim)
}
