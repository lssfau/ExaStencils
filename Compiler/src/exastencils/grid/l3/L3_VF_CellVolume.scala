package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain

/// L3_VF_CellVolume

case class L3_VF_CellVolume(
    var level : Int,
    var domain : L3_Domain
) extends L3_VirtualFieldWithScalar {

  override def name = "vf_cellVolume"
  override def knownAliases = ListBuffer("vf_cellVol")
  override def localization = L3_AtCellCenter
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      L3_VirtualFieldAccess(L3_VF_CellWidthPerDim(level, domain, dim), index) : L3_Expression
    ).reduce(_ * _)
  }
}
