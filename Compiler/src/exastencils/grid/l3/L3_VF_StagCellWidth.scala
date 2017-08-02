package exastencils.grid.l3

import scala.collection.mutable.ListBuffer

import exastencils.base.l3.L3_ImplicitConversion._
import exastencils.base.l3._
import exastencils.domain.l3.L3_Domain

/// L3_VF_StagCellWidthAsVec

case class L3_VF_StagCellWidthAsVec(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int
) extends L3_VirtualFieldWithVec {

  override def name = s"vf_stag_${ stagDim }_cellWidth"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def listPerDim = (0 until numDims).map(L3_VF_StagCellWidthPerDim(level, domain, stagDim, _) : L3_VirtualFieldPerDim).to[ListBuffer]
}

/// L3_VF_StagCellWidthPerDim

case class L3_VF_StagCellWidthPerDim(
    var level : Int,
    var domain : L3_Domain,
    var stagDim : Int,
    var dim : Int
) extends L3_VirtualFieldPerDim {

  override def name = s"vf_stag_${ stagDim }_cellWidth_$dim"
  override def knownAliases = ListBuffer("") // TODO
  override def localization = L3_AtFaceCenter(stagDim)
  override def resolutionPossible = true

  override def resolve(index : L3_ExpressionIndex) = {
    if (dim == stagDim)
      0.5 * (
        L3_VirtualFieldAccess(L3_VF_CellWidthPerDim(level, domain, dim), L3_GridUtil.offsetIndex(index, -1, dim))
          + L3_VirtualFieldAccess(L3_VF_CellWidthPerDim(level, domain, dim), index))
    else
      L3_VirtualFieldAccess(L3_VF_CellWidthPerDim(level, domain, dim), index)
  }
}
