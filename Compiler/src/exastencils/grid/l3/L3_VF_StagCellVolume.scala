package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain

/// L3_VF_StagCellVolume

case class L3_VF_StagCellVolume(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithScalar {

  override def name = s"vf_stag_${ stagDim }_cellVolume"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    (0 until domain.numDims).map(dim =>
      L3_VirtualFieldAccess(L3_VF_StagCellWidthPerDim(level, domain, stagDim, dim), index) : L3_Expression
    ).reduce(_ * _)
  }
}
